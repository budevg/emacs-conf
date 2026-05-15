;;; keymap-popup.el --- Described keymaps with popup help  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Version: 0.2.8
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; URL: https://codeberg.org/thanosapollo/emacs-keymap-popup

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Two macros: `keymap-popup-define' produces a real `defvar-keymap'
;; with embedded descriptions; `keymap-popup-annotate' adds popup
;; descriptions to an existing keymap.  `keymap-popup' displays
;; either as an interactive menu.  One definition, two uses:
;; direct key dispatch and popup help.
;;
;; The popup is a pure renderer: it reads state (buffer-locals, dynamic
;; descriptions) but owns none.  Commands mutate state in the user's
;; buffer; the popup just re-reads it on the next refresh.

;;; Code:

(require 'cl-lib)

(defgroup keymap-popup nil
  "Described keymaps with popup help."
  :group 'convenience)

(defcustom keymap-popup-display-action
  '(display-buffer-in-side-window (side . bottom))
  "Display action for the popup buffer.
Only used by `keymap-popup-backend-side-window'.
Common values:
  (display-buffer-in-side-window (side . bottom))  - frame-wide
  (display-buffer-below-selected)                  - current window only"
  :type display-buffer--action-custom-type
  :group 'keymap-popup)

(defcustom keymap-popup-backend #'keymap-popup-backend-side-window
  "Function returning a display backend plist (:show :fit :hide).
Each backend function receives a single buffer argument.
:show displays the buffer, :fit refits after content changes,
:hide removes the display and cleans up."
  :type '(choice (const :tag "Side window" keymap-popup-backend-side-window)
                 (const :tag "Child frame" keymap-popup-backend-child-frame)
                 function)
  :group 'keymap-popup)

(defcustom keymap-popup-child-frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . t)
    (min-height . t)
    (border-width . 0)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (no-other-window . t)
    (no-delete-other-windows . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Frame parameters for the child-frame backend.
These are merged with runtime parameters (parent-frame, minibuffer,
visibility) when creating the child frame."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'keymap-popup)

(defcustom keymap-popup-buffer-parameters
  '((buffer-read-only . t)
    (cursor-type . nil)
    (mode-line-format
     . (" "
        (:eval (and keymap-popup--active-exit-key
                    (propertize (format " %s " keymap-popup--active-exit-key)
                                'face 'keymap-popup-key)))
        " "
        (:eval (or keymap-popup--resolved-docstring ""))))
    (header-line-format . nil)
    (tab-line-format . nil)
    (left-margin-width . 1)
    (right-margin-width . 1))
  "Buffer-local parameters applied to the popup buffer.
Each entry is (VARIABLE . VALUE).  Users can remove entries to
keep defaults or change values to customize the popup appearance."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'keymap-popup)

(defcustom keymap-popup-persistent nil
  "When non-nil, the popup stays open after every key press.
All suffix commands execute and refresh the popup in place.
Only the exit key and \\`C-g' dismiss it.
Can also be set per-keymap via the `:persistent' keyword in
`keymap-popup-define' or `keymap-popup-annotate'."
  :type 'boolean
  :group 'keymap-popup)

(defcustom keymap-popup-default-popup-key "h"
  "Default key to open the popup in keymaps defined with `keymap-popup-define'.
Applied automatically by `keymap-popup-define' when :popup-key is
not specified.  Not applied by `keymap-popup-annotate' (must be
given explicitly)."
  :type 'key
  :group 'keymap-popup)

(defcustom keymap-popup-default-exit-key "q"
  "Default key to dismiss the popup.
Applied automatically by `keymap-popup-define' when :exit-key is
not specified.  For `keymap-popup-annotate', used as runtime
fallback when :exit-key is omitted."
  :type 'key
  :group 'keymap-popup)

;;; Faces

(defface keymap-popup-key
  '((t :inherit help-key-binding))
  "Face for key bindings in the popup.")

(defface keymap-popup-group-header
  '((t :weight bold))
  "Face for group headers in the popup.")

(defface keymap-popup-value
  '((t :inherit font-lock-string-face :weight bold))
  "Face for switch values in the popup.")

(defface keymap-popup-submenu
  '((t :inherit font-lock-type-face))
  "Face for sub-menu entries in the popup.")

(defface keymap-popup-inapt
  '((t :inherit shadow))
  "Face for inapt (disabled) entries in the popup.")

;;; Keymap metadata

(defun keymap-popup--meta (keymap prop)
  "Get popup metadata PROP from KEYMAP via pseudo-key lookup."
  (let ((val (lookup-key keymap (vector 'keymap-popup prop))))
    ;; lookup-key returns an integer when the vector is a prefix of a
    ;; longer binding rather than an exact match.  Filter that out.
    (and (not (numberp val)) val)))

;; Values are stored via define-key, so t cannot be used as a value
;; (it means "default binding").  Use symbols like 'yes instead.
(gv-define-setter keymap-popup--meta (val keymap prop)
  `(define-key ,keymap (vector 'keymap-popup ,prop) ,val))

;;; Parsers

(defun keymap-popup--extract-props (plist)
  "Extract known properties from PLIST.
Recognized keys: :if, :inapt-if, :stay-open, :c-u."
  (cl-loop for (k v) on plist by #'cddr
           when (memq k '(:if :inapt-if :stay-open :c-u))
           append (list k v)))

(defun keymap-popup--parse-entry (key spec)
  "Parse binding SPEC for KEY into a plist.
KEY is a key string for normal entries, or a command symbol for
annotated entries.  SPEC is (DESCRIPTION COMMAND-OR-TYPE &rest PROPS)
for key-based entries, or (DESCRIPTION &rest PROPS) for annotated ones."
  (if (symbolp key)
      ;; Annotated entry: key is a command symbol, spec is (DESC . PROPS),
      ;; any atom, or a bare lambda/function form.
      (let* ((spec (cond ((not (consp spec)) (list spec))
                         ((memq (car-safe spec) '(lambda function)) (list spec))
                         (t spec)))
             (description (car spec))
             (props (cdr spec)))
        `(:key nil :description ,description :type suffix
               :command ,key
               ,@(keymap-popup--extract-props props)))
    ;; Normal entry: key is a string
    (let* ((description (car spec))
           (second (cadr spec))
           (rest (cddr spec)))
      (pcase second
        (:switch
         `(:key ,key :description ,description :type switch
                :variable ,(car rest)
                ,@(keymap-popup--extract-props (cdr rest))))
        (:keymap
         `(:key ,key :description ,description :type keymap
                :target ,(car rest)
                ,@(keymap-popup--extract-props (cdr rest))))
        (_
         `(:key ,key :description ,description :type suffix
                :command ,second
                ,@(keymap-popup--extract-props rest)))))))

(defun keymap-popup--split-groups (bindings)
  "Split BINDINGS at :group and :row keywords.
Returns a list of rows, each row a list of (NAME . FLAT-ENTRIES) chunks.
`:group' starts a new group within the current row.
`:row' starts a new row."
  (keymap-popup--split-groups-1 bindings nil nil nil nil))

(defun keymap-popup--split-groups-1 (rest name entries groups rows)
  "Recursive helper for `keymap-popup--split-groups'.
REST is remaining bindings, NAME is current group name, ENTRIES
is accumulated entries (reversed), GROUPS is current row's groups
\(reversed), ROWS is accumulated rows (reversed)."
  (let ((flush-group (if entries
                         (cons (cons name (reverse entries)) groups)
                       groups)))
    (cond
     ((null rest)
      (reverse (if flush-group
                   (cons (reverse flush-group) rows)
                 rows)))
     ((eq (car rest) :row)
      (keymap-popup--split-groups-1
       (cdr rest) nil nil nil
       (if flush-group (cons (reverse flush-group) rows) rows)))
     ((eq (car rest) :group)
      (keymap-popup--split-groups-1
       (cddr rest) (cadr rest) nil flush-group rows))
     (t
      (keymap-popup--split-groups-1
       (cddr rest) name
       (cons (cons (car rest) (cadr rest)) entries)
       groups rows)))))

(defun keymap-popup--parse-group-name (raw)
  "Parse RAW group name into (NAME . PROPS).
RAW is a string, a lambda, or a list (NAME :if PRED :inapt-if PRED).
A list whose car is not `lambda' is treated as a name with properties."
  (if (and (consp raw) (not (eq (car raw) 'lambda)))
      (cons (car raw) (keymap-popup--extract-props (cdr raw)))
    (cons raw nil)))

(defun keymap-popup--parse-chunk (chunk)
  "Parse CHUNK of (NAME . ((KEY . SPEC) ...)) into a group plist."
  (let* ((name-props (keymap-popup--parse-group-name (car chunk)))
         (name (car name-props))
         (group-props (cdr name-props))
         (entries (mapcar (lambda (pair)
                            (keymap-popup--parse-entry (car pair) (cdr pair)))
                          (cdr chunk))))
    `(:name ,name :entries ,entries ,@group-props)))

(defun keymap-popup--parse-bindings (bindings)
  "Parse BINDINGS into a list of rows.
Each row is a list of group plists with :name and :entries."
  (mapcar (lambda (row) (mapcar #'keymap-popup--parse-chunk row))
          (keymap-popup--split-groups bindings)))

;;; Infix generators

(defun keymap-popup--create-switch (map-name variable description)
  "Create buffer-local VARIABLE with toggle command for MAP-NAME.
DESCRIPTION is used in the toggle message."
  (defvar-1 variable nil)
  (make-variable-buffer-local variable)
  (defalias (intern (format "%s--toggle-%s" map-name variable))
    (lambda ()
      (:documentation (format "Toggle %s." description))
      (interactive)
      (set variable (not (symbol-value variable)))
      (message "%s: %s" description (if (symbol-value variable) "on" "off")))))

(defun keymap-popup--entry-command (map-name entry)
  "Return the command to bind in MAP-NAME's keymap for ENTRY."
  (pcase-exhaustive (plist-get entry :type)
    ('suffix (plist-get entry :command))
    ('switch (intern (format "%s--toggle-%s" map-name (plist-get entry :variable))))
    ('keymap (let ((target (plist-get entry :target)))
               (lambda () (interactive) (keymap-popup target))))))

;;; Macro helpers

(defun keymap-popup--build-keymap-pairs (map-name entries)
  "Build flat key/command list for `defvar-keymap' from ENTRIES.
MAP-NAME is used to derive generated command names."
  (cl-loop for entry in entries
           for cmd = (keymap-popup--entry-command map-name entry)
           append (list (plist-get entry :key)
                        (if (symbolp cmd) `#',cmd cmd))))

(defun keymap-popup--build-entry-form (entry)
  "Build a `list' form for a single ENTRY."
  (let* ((type (plist-get entry :type))
         (type-props (pcase-exhaustive type
                       ('suffix (let ((cmd (plist-get entry :command)))
                                  `(:command ,(if (symbolp cmd) `#',cmd cmd)
                                             ,@(and (plist-get entry :stay-open)
                                                    '(:stay-open t)))))
                       ('keymap `(:target ,(plist-get entry :target)))
                       ('switch `(:variable ',(plist-get entry :variable)))))
         (if-pred (plist-get entry :if))
         (inapt-if (plist-get entry :inapt-if)))
    `(list :key ,(plist-get entry :key)
           :description ,(plist-get entry :description)
           :type ',type
           ,@type-props
           ,@(and if-pred (list :if if-pred))
           ,@(and inapt-if (list :inapt-if inapt-if))
           ,@(and-let* ((c-u (plist-get entry :c-u)))
               (list :c-u c-u)))))

(defun keymap-popup--build-descriptions-form (rows)
  "Build a `list' form that constructs descriptions at load time.
ROWS is a list of rows, each row a list of groups.
Uses list calls so lambdas get compiled."
  `(list ,@(mapcar
            (lambda (row)
              `(list ,@(mapcar
                        (lambda (group)
                          (let ((if-pred (plist-get group :if))
                                (inapt-if (plist-get group :inapt-if)))
                            `(list :name ,(plist-get group :name)
                                   :entries (list ,@(mapcar #'keymap-popup--build-entry-form
                                                            (plist-get group :entries)))
                                   ,@(and if-pred (list :if if-pred))
                                   ,@(and inapt-if (list :inapt-if inapt-if)))))
                        row)))
            rows)))

;;; Macro

(defun keymap-popup--consume-keywords (rest keywords)
  "Consume KEYWORDS from REST in any order.
Returns (VALUES . REMAINING) where VALUES is a list of extracted
values (nil for absent keywords), ordered as KEYWORDS."
  (named-let collect ((rest rest) (alist nil))
    (if (and rest (memq (car rest) keywords))
        (collect (cddr rest)
                 (cons (cons (car rest) (cadr rest)) alist))
      (cons (mapcar (lambda (kw) (alist-get kw alist)) keywords)
            rest))))

(defun keymap-popup--extract-macro-opts (body)
  "Extract macro options from BODY.
Returns (DOCSTRING POPUP-KEY EXIT-KEY PARENT DESCRIPTION PERSISTENT BINDINGS).
Unspecified keywords yield nil."
  (let* ((docstring (and (stringp (car body))
                         (or (null (cadr body))
                             (not (listp (cadr body))))
                         (car body)))
         (rest (if docstring (cdr body) body))
         (result (keymap-popup--consume-keywords
                  rest '(:popup-key :exit-key :parent :description :persistent))))
    (append (list docstring) (car result) (list (cdr result)))))

;;;###autoload
(defmacro keymap-popup-define (name &rest body)
  "Define NAME as a keymap with embedded descriptions.
BODY is an optional docstring, optional :popup-key KEY (default
per `keymap-popup-default-popup-key'), optional :exit-key KEY
\(default per `keymap-popup-default-exit-key'), optional :parent
KEYMAP, optional :description STRING-OR-FUNCTION, optional
:persistent BOOL, followed by :group keywords and KEY (DESC ...)
pairs."
  (declare (indent 1))
  (pcase-let* ((`(,docstring ,popup-key ,exit-key ,parent ,description
                             ,persistent ,bindings)
                (keymap-popup--extract-macro-opts body))
               (popup-key (or popup-key keymap-popup-default-popup-key))
               (exit-key (or exit-key keymap-popup-default-exit-key))
               (rows (keymap-popup--parse-bindings bindings))
               (all-entries (cl-loop for row in rows
				     append (cl-loop for group in row
						     append (plist-get group :entries))))
               (switch-entries (cl-loop for entry in all-entries
					when (eq (plist-get entry :type) 'switch)
					collect entry))
               (keymap-pairs (keymap-popup--build-keymap-pairs name all-entries)))
    `(progn
       ,@(mapcar (lambda (e)
                   `(keymap-popup--create-switch
                     ',name ',(plist-get e :variable)
                     ,(plist-get e :description)))
                 switch-entries)
       (defvar-keymap ,name
         ,@(and docstring (list :doc docstring))
         ,@(and parent (list :parent parent))
         ,@keymap-pairs
         ,popup-key (lambda () (interactive) (keymap-popup ,name)))
       (setf (keymap-popup--meta ,name 'descriptions)
             ,(keymap-popup--build-descriptions-form rows))
       (setf (keymap-popup--meta ,name 'exit-key) ,exit-key)
       ,@(and description
              `((setf (keymap-popup--meta ,name 'description) ,description)))
       ,@(and persistent
              `((setf (keymap-popup--meta ,name 'persistent) 'yes))))))

;;;###autoload
(defmacro keymap-popup-annotate (keymap &rest body)
  "Annotate existing KEYMAP with popup descriptions.
BODY is optional :popup-key KEY, optional :exit-key KEY, optional
:description STRING-OR-FUNCTION, optional :persistent BOOL,
followed by :group keywords and COMMAND-SYMBOL DESCRIPTION pairs.
COMMAND-SYMBOL is a function symbol already bound in the keymap.
DESCRIPTION is a string or (STRING &rest PROPS).

Unlike `keymap-popup-define', no defaults are applied for
:popup-key or :exit-key.  Only explicitly provided values take
effect.  When :exit-key is omitted, the popup falls back to
`keymap-popup-default-exit-key' at display time.

Keys are resolved dynamically via `where-is-internal' at display
time, so the popup always reflects the user's current bindings."
  (declare (indent 1))
  (pcase-let* ((`(,_docstring ,popup-key ,exit-key ,_parent ,description
                              ,persistent ,bindings)
                (keymap-popup--extract-macro-opts body))
               (rows (keymap-popup--parse-bindings bindings)))
    `(progn
       (setf (keymap-popup--meta ,keymap 'descriptions)
             ,(keymap-popup--build-descriptions-form rows))
       ,@(and popup-key
              `((keymap-set ,keymap ,popup-key
                            (lambda () (interactive) (keymap-popup ,keymap)))))
       ,@(and exit-key
              `((setf (keymap-popup--meta ,keymap 'exit-key) ,exit-key)))
       ,@(and description
              `((setf (keymap-popup--meta ,keymap 'description) ,description)))
       ,@(and persistent
              `((setf (keymap-popup--meta ,keymap 'persistent) 'yes))))))

;;; Public API

(defun keymap-popup--map-groups (rows fn)
  "Apply FN to each group in ROWS, returning the transformed rows.
FN receives a group plist and returns a new group plist."
  (mapcar (lambda (row) (mapcar fn row)) rows))

(defun keymap-popup--add-entry-to-rows (rows entry group-name)
  "Return ROWS with ENTRY appended to the group named GROUP-NAME.
Falls back to the first group if GROUP-NAME is not found."
  (let ((target (or (cl-loop for row in rows
                             thereis (cl-loop for g in row
                                              when (equal (plist-get g :name) group-name)
                                              return group-name))
                    (plist-get (caar rows) :name))))
    (keymap-popup--map-groups
     rows
     (lambda (group)
       (if (equal (plist-get group :name) target)
           (list :name (plist-get group :name)
                 :entries (append (plist-get group :entries) (list entry)))
         group)))))

(defun keymap-popup--remove-key-from-rows (rows key)
  "Return ROWS with entries matching KEY filtered out."
  (keymap-popup--map-groups
   rows
   (lambda (group)
     (list :name (plist-get group :name)
           :entries (cl-remove-if
                     (lambda (e) (equal (plist-get e :key) key))
                     (plist-get group :entries))))))

;;;###autoload
(defun keymap-popup-add-entry (keymap key description command &optional group)
  "Add KEY binding with DESCRIPTION and COMMAND to KEYMAP.
GROUP is the group name to add to (nil for the first group).
Updates both the keymap and the popup descriptions."
  (let ((descs (keymap-popup--meta keymap 'descriptions)))
    (or descs (user-error "No descriptions in keymap"))
    (keymap-set keymap key command)
    (let ((entry (list :key key :description description
                       :type 'suffix :command command)))
      (setf (keymap-popup--meta keymap 'descriptions)
            (keymap-popup--add-entry-to-rows descs entry group)))))

;;;###autoload
(defun keymap-popup-remove-entry (keymap key)
  "Remove KEY binding from KEYMAP.
Updates both the keymap and the popup descriptions."
  (keymap-set keymap key nil)
  (setf (keymap-popup--meta keymap 'descriptions)
        (keymap-popup--remove-key-from-rows
         (keymap-popup--meta keymap 'descriptions) key)))

;;; Renderer

(defun keymap-popup--resolve-description (desc)
  "If DESC is a function, call it; otherwise return as-is."
  (if (functionp desc) (funcall desc) desc))

(defun keymap-popup--render-entry (entry &optional prefix-mode key-width)
  "Render ENTRY into a formatted line, or nil if :if hides it.
When PREFIX-MODE is non-nil, entries with :c-u are highlighted and
their :c-u description is shown; other entries are dimmed.
KEY-WIDTH pads the key column for alignment."
  (when (or (null (plist-get entry :if))
            (funcall (plist-get entry :if)))
    (let* ((inapt (and-let* ((pred (plist-get entry :inapt-if)))
                    (funcall pred)))
           (raw-desc (keymap-popup--resolve-description
                      (plist-get entry :description)))
           (type (plist-get entry :type))
           (desc (if (eq type 'keymap)
                     (propertize raw-desc 'face 'keymap-popup-submenu)
                   raw-desc))
           (c-u-desc (plist-get entry :c-u))
           (raw-key (plist-get entry :key))
           (padded-key (if key-width
                           (string-pad raw-key key-width)
                         raw-key))
           (key-str (propertize padded-key 'face 'keymap-popup-key))
           (value-str (if (eq type 'switch)
                          (propertize
                           (if (symbol-value (plist-get entry :variable))
                               " [on]" " [off]")
                           'face 'keymap-popup-value)
			""))
           (c-u-str (and c-u-desc
                         (if prefix-mode
                             (propertize (format " (%s)" c-u-desc)
                                         'face 'warning)
                           (propertize (format " (%s)" c-u-desc)
                                       'face 'shadow))))
           (line (format "  %s  %s%s%s" key-str desc value-str
                         (or c-u-str ""))))
      (cond
       (inapt (propertize line 'face 'keymap-popup-inapt))
       ((and prefix-mode (not c-u-desc))
        (propertize line 'face 'shadow))
       (t line)))))

(defun keymap-popup--render-group-lines (group &optional prefix-mode)
  "Render GROUP into a list of lines (strings).
When PREFIX-MODE is non-nil, pass it to entry rendering.
Returns nil if the group is hidden by :if or has no visible entries.
When the group has :inapt-if that returns non-nil, all entries are
rendered with the inapt face."
  (when (or (null (plist-get group :if))
            (funcall (plist-get group :if)))
    (let* ((group-inapt (and-let* ((pred (plist-get group :inapt-if)))
                          (funcall pred)))
           (entries (plist-get group :entries))
           (key-width (cl-loop for entry in entries
                               maximize (length (plist-get entry :key))))
           (header (and-let* ((raw-name (plist-get group :name))
                              (name (keymap-popup--resolve-description raw-name)))
                     (propertize name 'face (if group-inapt
                                                'keymap-popup-inapt
                                              'keymap-popup-group-header))))
           (lines (cl-loop for entry in entries
                           for line = (keymap-popup--render-entry
                                       entry prefix-mode key-width)
                           when line collect line)))
      (when lines
        (let ((result (if header (cons header lines) lines)))
          (if group-inapt
              (mapcar (lambda (line) (propertize line 'face 'keymap-popup-inapt))
                      result)
            result))))))

(defun keymap-popup--string-width-visible (str)
  "Return the visible width of STR, ignoring text properties."
  (string-width (substring-no-properties str)))

(defun keymap-popup--column-width (col)
  "Return the max visible width of lines in COL."
  (cl-loop for line in col
           maximize (keymap-popup--string-width-visible line)))

(defun keymap-popup--join-columns (columns separator col-widths)
  "Join COLUMNS side by side with SEPARATOR between them.
COL-WIDTHS is a list of minimum widths per column position.
Shorter columns are padded with blank lines."
  (let* ((max-height (cl-loop for col in columns maximize (length col)))
         (padded-cols (cl-mapcar
                       (lambda (col width)
                         (let ((padded (mapcar (lambda (line)
                                                 (string-pad line width))
                                               col))
                               (blanks (make-list (- max-height (length col))
                                                  (make-string width ?\s))))
                           (append padded blanks)))
                       columns col-widths)))
    (cl-loop for row from 0 below max-height
             collect (string-trim-right
                      (mapconcat (lambda (col) (nth row col))
                                 padded-cols
                                 separator)))))

(defun keymap-popup--rows-to-columns (rows &optional prefix-mode)
  "Render each row of ROWS into its list of column line-lists.
When PREFIX-MODE is non-nil, pass it to group rendering.
Returns a list of ((col-lines ...) ...) per row, filtering empty groups."
  (mapcar (lambda (row)
            (cl-loop for group in row
                     when (keymap-popup--render-group-lines group prefix-mode)
                     collect it))
          rows))

(defun keymap-popup--global-col-widths (rendered-rows)
  "Compute max column width per position across all RENDERED-ROWS."
  (let ((max-cols (cl-loop for cols in rendered-rows
                           maximize (length cols))))
    (cl-loop for i from 0 below max-cols
             collect (cl-loop for cols in rendered-rows
                              when (nth i cols)
                              maximize (keymap-popup--column-width (nth i cols))))))

(defun keymap-popup--render (rows &optional prefix-mode)
  "Render ROWS into a complete popup string.
ROWS is a list of rows, each row a list of groups.
When PREFIX-MODE is non-nil, highlight :c-u entries and dim others.
Column widths are aligned across all rows."
  (let* ((rendered-rows (keymap-popup--rows-to-columns rows prefix-mode))
         (col-widths (keymap-popup--global-col-widths rendered-rows))
         (sections (cl-loop for cols in rendered-rows
                            when cols
                            collect (string-join
                                     (keymap-popup--join-columns
                                      cols "   " col-widths)
                                     "\n"))))
    (concat (string-join sections "\n") "\n")))

;;; Popup state

(defvar-local keymap-popup--source-buffer nil
  "The buffer from which the popup was invoked.
Switch variables are buffer-local there, so rendering must read
`symbol-value' in that buffer's context.")
(defvar-local keymap-popup--active-keymap nil
  "The currently displayed keymap in the popup.")
(defvar-local keymap-popup--active-descriptions nil
  "Descriptions for the currently active keymap.")
(defvar-local keymap-popup--active-docstring nil
  "Docstring for the currently active keymap.")
(defvar-local keymap-popup--stack nil
  "Stack of parent state plists for sub-menu navigation.")
(defvar-local keymap-popup--prefix-mode nil
  "Non-nil when \\`C-u' prefix mode is active.")
(defvar-local keymap-popup--reentering nil
  "Non-nil when a sub-menu just popped, preventing cascading exit.")
(defvar-local keymap-popup--active-exit-key nil
  "The exit key for the currently active popup.")
(defvar-local keymap-popup--resolved-docstring nil
  "Resolved docstring string for mode-line display.")
(defvar-local keymap-popup--persistent nil
  "Non-nil when this popup instance is in persistent mode.")
(defvar-local keymap-popup--wrapper-map nil
  "The active wrapper keymap on `overriding-terminal-local-map'.")
(defvar-local keymap-popup--display-backend nil
  "The active display backend plist (:show :fit :hide).")

;;; Popup display

(defun keymap-popup--collect-descriptions (keymap)
  "Collect descriptions from KEYMAP and all its parent keymaps.
Walks the native parent chain via `keymap-parent'."
  (cl-loop for map = keymap then (keymap-parent map)
           while map
           when (keymap-popup--meta map 'descriptions)
           append it))

(defun keymap-popup--find-entry-by-key (descriptions key-str)
  "Find the entry matching KEY-STR in DESCRIPTIONS.
DESCRIPTIONS is a list of rows, each row a list of groups.
Returns the entry plist, or nil."
  (cl-loop for row in descriptions
           thereis (cl-loop for group in row
                            thereis (cl-loop for entry in (plist-get group :entries)
                                             when (equal (plist-get entry :key) key-str)
                                             return entry))))

(defun keymap-popup--infix-p (descriptions key-str)
  "Return non-nil if KEY-STR maps to a switch entry in DESCRIPTIONS."
  (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str)))
    (eq (plist-get entry :type) 'switch)))

(defun keymap-popup--keymap-target (descriptions key-str)
  "Return the target map symbol if KEY-STR is a :keymap entry in DESCRIPTIONS."
  (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str))
             (_ (eq (plist-get entry :type) 'keymap)))
    (plist-get entry :target)))

(defun keymap-popup--find-group-for-key (descriptions key-str)
  "Find the group containing KEY-STR in DESCRIPTIONS."
  (cl-loop for row in descriptions
           thereis (cl-loop for group in row
                            when (cl-loop for entry in (plist-get group :entries)
                                          thereis (equal (plist-get entry :key) key-str))
                            return group)))

(defun keymap-popup--inapt-p (descriptions key-str)
  "Return non-nil if KEY-STR is inapt in DESCRIPTIONS.
Checks both group-level and entry-level :inapt-if predicates."
  (or (and-let* ((group (keymap-popup--find-group-for-key descriptions key-str))
                 (pred (plist-get group :inapt-if)))
        (funcall pred))
      (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str))
                 (pred (plist-get entry :inapt-if)))
        (funcall pred))))

(defun keymap-popup--stay-open-p (descriptions key-str)
  "Return non-nil if KEY-STR should keep the popup open in DESCRIPTIONS.
True for switches and suffixes with :stay-open."
  (and-let* ((entry (keymap-popup--find-entry-by-key descriptions key-str)))
    (or (eq (plist-get entry :type) 'switch)
        (plist-get entry :stay-open))))

(defun keymap-popup--keep-popup-p (descriptions key-str)
  "Return non-nil if KEY-STR should keep the popup open in DESCRIPTIONS.
True for switches, stay-open suffixes, inapt keys, and :keymap entries."
  (or (keymap-popup--infix-p descriptions key-str)
      (keymap-popup--stay-open-p descriptions key-str)
      (keymap-popup--inapt-p descriptions key-str)
      (keymap-popup--keymap-target descriptions key-str)))

(defun keymap-popup--refresh-buffer (buf descriptions &optional prefix-mode)
  "Re-render popup BUF with DESCRIPTIONS, refit via backend.
PREFIX-MODE toggles prefix argument highlighting."
  (let ((content (keymap-popup--render descriptions prefix-mode)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))
    (when-let* ((fit (plist-get (buffer-local-value 'keymap-popup--display-backend buf)
                                :fit)))
      (funcall fit buf))))

(defun keymap-popup--refresh (buf)
  "Re-render popup BUF from its buffer-local state.
Renders in the source buffer's context so `symbol-value' for
switch variables sees the user's buffer-local values.
Resolves the docstring for mode-line display."
  (when (buffer-live-p buf)
    (let ((source (buffer-local-value 'keymap-popup--source-buffer buf))
          (descs (buffer-local-value 'keymap-popup--active-descriptions buf))
          (doc (buffer-local-value 'keymap-popup--active-docstring buf))
          (prefix (buffer-local-value 'keymap-popup--prefix-mode buf)))
      (with-current-buffer (if (buffer-live-p source) source buf)
        (let ((resolved (and doc (keymap-popup--resolve-description doc))))
          (with-current-buffer buf
            (setq-local keymap-popup--resolved-docstring resolved)))
        (keymap-popup--refresh-buffer buf descs prefix)))))

(defun keymap-popup--resolve-key (entry keymap)
  "Resolve ENTRY's :command to a key in KEYMAP.
Returns entry with :key filled in, or nil if unbound."
  (if (plist-get entry :key) entry
    (and-let* ((cmd (plist-get entry :command))
               (keys (where-is-internal cmd keymap t)))
      (plist-put (copy-sequence entry) :key (key-description keys)))))

(defun keymap-popup--resolve-descriptions (rows keymap)
  "Resolve command symbols to keys in ROWS using KEYMAP.
Drops entries whose command has no binding."
  (keymap-popup--map-groups
   rows
   (lambda (group)
     (plist-put (copy-sequence group) :entries
                (cl-loop for entry in (plist-get group :entries)
                         when (keymap-popup--resolve-key entry keymap)
                         collect it)))))

;;; Display backends

(defun keymap-popup--show-side-window (buf)
  "Display BUF in a side window."
  (display-buffer buf (append keymap-popup-display-action
                              '((window-height . fit-window-to-buffer))))
  (when-let* ((win (get-buffer-window buf)))
    (fit-window-to-buffer win)))

(defun keymap-popup--fit-side-window (buf)
  "Refit the side window displaying BUF."
  (when-let* ((win (get-buffer-window buf))
              (_ (window-live-p win)))
    (fit-window-to-buffer win)))

(defun keymap-popup--hide-side-window (buf)
  "Delete the side window displaying BUF."
  (when-let* ((win (get-buffer-window buf)))
    (delete-window win)))

(defun keymap-popup--show-child-frame (buf)
  "Display BUF in a child frame centered on the parent.
Frame parameters are taken from `keymap-popup-child-frame-parameters'."
  (let* ((parent (selected-frame))
         (after-make-frame-functions nil)
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (minibuffer . ,(minibuffer-window))
                   (visibility . nil)
                   ,@keymap-popup-child-frame-parameters)))
         (win (frame-root-window frame)))
    (set-window-buffer win buf)
    (set-window-dedicated-p win t)
    (fit-frame-to-buffer frame)
    (let ((x (/ (- (frame-pixel-width parent) (frame-pixel-width frame)) 2))
          (y (/ (- (frame-pixel-height parent) (frame-pixel-height frame)) 2)))
      (set-frame-position frame (max 0 x) (max 0 y)))
    (make-frame-visible frame)
    (redirect-frame-focus frame parent)))

(defun keymap-popup--fit-child-frame (buf)
  "Refit the child frame displaying BUF."
  (when-let* ((win (get-buffer-window buf t))
              (frame (window-frame win))
              (_ (frame-parent frame)))
    (fit-frame-to-buffer frame)))

(defun keymap-popup--hide-child-frame (buf)
  "Delete the child frame displaying BUF."
  (when-let* ((win (get-buffer-window buf t))
              (frame (window-frame win))
              (_ (frame-parent frame)))
    (delete-frame frame)))

(defun keymap-popup-backend-side-window ()
  "Return a side-window display backend."
  (list :show #'keymap-popup--show-side-window
        :fit  #'keymap-popup--fit-side-window
        :hide #'keymap-popup--hide-side-window))

(defun keymap-popup-backend-child-frame ()
  "Return a child-frame display backend."
  (list :show #'keymap-popup--show-child-frame
        :fit  #'keymap-popup--fit-child-frame
        :hide #'keymap-popup--hide-child-frame))

(defun keymap-popup--prepare-buffer ()
  "Create and configure the popup buffer."
  (let ((buf (get-buffer-create "*keymap-popup*")))
    (with-current-buffer buf
      (pcase-dolist (`(,var . ,val) keymap-popup-buffer-parameters)
        (set (make-local-variable var) val)))
    buf))

(defun keymap-popup--suspend ()
  "Suspend the popup's transient map for minibuffer input."
  (when-let* ((buf (get-buffer "*keymap-popup*"))
              (map (buffer-local-value 'keymap-popup--wrapper-map buf)))
    (internal-pop-keymap map 'overriding-terminal-local-map)))

(defun keymap-popup--resume ()
  "Resume the popup's transient map after minibuffer input."
  (when-let* ((buf (get-buffer "*keymap-popup*"))
              (map (buffer-local-value 'keymap-popup--wrapper-map buf)))
    (internal-push-keymap map 'overriding-terminal-local-map)))

(defun keymap-popup--teardown (buf)
  "Remove the popup display for BUF and kill it."
  (when (buffer-live-p buf)
    (remove-hook 'minibuffer-setup-hook #'keymap-popup--suspend)
    (remove-hook 'minibuffer-exit-hook #'keymap-popup--resume)
    (when-let* ((hide (plist-get (buffer-local-value 'keymap-popup--display-backend buf)
                                 :hide)))
      (funcall hide buf))
    (kill-buffer buf)))

(defun keymap-popup--make-keep-pred (buf)
  "Return a keep-pred for `set-transient-map'.
Reads state from BUF.  Consumes the reentering flag on read."
  (lambda ()
    (and (buffer-live-p buf)
         (let ((key-str (key-description (this-command-keys-vector)))
               (exit-key (buffer-local-value 'keymap-popup--active-exit-key buf)))
           (cond
            ((active-minibuffer-window) t)
            ((buffer-local-value 'keymap-popup--reentering buf)
             (with-current-buffer buf
               (setq-local keymap-popup--reentering nil))
             t)
            ((memq this-command
                   '(universal-argument universal-argument-more
					digit-argument negative-argument
					keymap-popup--prefix-argument
					describe-key describe-key-briefly)))
            ((equal key-str exit-key) nil)
            ((eq this-command 'keyboard-quit) nil)
            ((buffer-local-value 'keymap-popup--persistent buf))
            (t (and-let* ((descs (buffer-local-value
                                  'keymap-popup--active-descriptions buf)))
                 (keymap-popup--keep-popup-p descs key-str))))))))


(defun keymap-popup--make-on-exit (buf)
  "Return an on-exit callback for `set-transient-map' closing BUF.
Pops the sub-menu stack if exit-key or \\`C-g' caused the exit,
otherwise tears down completely."
  (lambda ()
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((key-str (key-description (this-command-keys-vector))))
          (if (and keymap-popup--stack
                   (or (equal key-str keymap-popup--active-exit-key)
                       (equal key-str "C-g")))
              (pcase-let ((`(:keymap ,km :descriptions ,descs :docstring ,doc
                                     :exit-key ,ek)
                           (pop keymap-popup--stack)))
                (setq-local keymap-popup--active-keymap km
                            keymap-popup--active-descriptions descs
                            keymap-popup--active-docstring doc
                            keymap-popup--active-exit-key ek
                            keymap-popup--reentering t
                            keymap-popup--prefix-mode nil)
                (keymap-popup--refresh buf))
            (keymap-popup--teardown buf)))))))

(defun keymap-popup--collect-entries (descriptions fn)
  "Collect non-nil results of (FN ENTRY GROUP) across DESCRIPTIONS.
Walks rows, groups, and entries.  FN receives an entry plist and
its parent group plist; non-nil return values are collected."
  (mapcan (lambda (row)
            (mapcan (lambda (group)
                      (mapcan (lambda (entry)
                                (and-let* ((result (funcall fn entry group)))
                                  (list result)))
                              (plist-get group :entries)))
                    row))
          descriptions))

(defun keymap-popup--classify-entries (descriptions)
  "Walk DESCRIPTIONS once, classify entries by type and properties.
Returns plist (:inapt KEYS :switches KEYS :submenus PAIRS :stay-open KEYS)."
  (let ((entries (keymap-popup--collect-entries
                  descriptions
                  (lambda (entry group)
                    (and-let* ((key (plist-get entry :key)))
                      (list :key key
                            :type (plist-get entry :type)
                            :target (plist-get entry :target)
                            :inapt (or (plist-get entry :inapt-if)
                                       (plist-get group :inapt-if))
                            :stay-open (plist-get entry :stay-open)))))))
    (list :inapt (cl-loop for e in entries
                          when (plist-get e :inapt)
                          collect (plist-get e :key))
          :switches (cl-loop for e in entries
                             when (eq (plist-get e :type) 'switch)
                             collect (plist-get e :key))
          :submenus (cl-loop for e in entries
                             when (eq (plist-get e :type) 'keymap)
                             collect (cons (plist-get e :key)
                                           (plist-get e :target)))
          :stay-open (cl-loop for e in entries
                              when (and (eq (plist-get e :type) 'suffix)
                                        (plist-get e :stay-open))
                              collect (plist-get e :key)))))

(defun keymap-popup--push-submenu (buf child-keymap)
  "Push current popup state in BUF and activate CHILD-KEYMAP's transient map."
  (with-current-buffer buf
    (push (list :keymap keymap-popup--active-keymap
                :descriptions keymap-popup--active-descriptions
                :docstring keymap-popup--active-docstring
                :exit-key keymap-popup--active-exit-key)
          keymap-popup--stack)
    (let* ((descs (keymap-popup--resolve-descriptions
                   (keymap-popup--collect-descriptions child-keymap)
                   child-keymap))
           (doc (keymap-popup--meta child-keymap 'description))
           (exit-key (or (keymap-popup--meta child-keymap 'exit-key)
                         keymap-popup-default-exit-key)))
      (setq-local keymap-popup--active-keymap child-keymap
                  keymap-popup--active-descriptions descs
                  keymap-popup--active-docstring doc
                  keymap-popup--active-exit-key exit-key
                  keymap-popup--prefix-mode nil)
      (keymap-popup--refresh buf)
      (let ((wrapper (keymap-popup--build-wrapper-map child-keymap descs buf exit-key)))
        (setq-local keymap-popup--wrapper-map wrapper)
        (set-transient-map wrapper
                           (keymap-popup--make-keep-pred buf)
                           (keymap-popup--make-on-exit buf))))))

(defun keymap-popup--prefix-argument ()
  "Toggle prefix argument mode in the active popup.
When toggling on, activates `universal-argument-map' so that
subsequent digit and `negative-argument' keys refine the prefix."
  (interactive)
  (when-let* ((buf (get-buffer "*keymap-popup*")))
    (with-current-buffer buf
      (setq-local keymap-popup--prefix-mode
                  (not keymap-popup--prefix-mode))
      (setq prefix-arg
            (when keymap-popup--prefix-mode '(4))))
    (keymap-popup--refresh buf)
    (when (buffer-local-value 'keymap-popup--prefix-mode buf)
      (universal-argument--mode))))

(defun keymap-popup--core-overrides (exit-key)
  "Return alist of core overrides for EXIT-KEY and prefix toggle."
  (list (cons exit-key
              (lambda () (interactive)))
        (cons "C-u" #'keymap-popup--prefix-argument)))

(defun keymap-popup--with-inapt-guard (buf key-str cmd)
  "Wrap CMD with a dynamic inapt check for KEY-STR in BUF.
When inapt, blocks execution and preserves `prefix-arg'.
When not inapt, calls CMD."
  (lambda () (interactive)
    (let ((descs (buffer-local-value 'keymap-popup--active-descriptions buf)))
      (if (keymap-popup--inapt-p descs key-str)
          (progn
            (message "Command unavailable")
            (when (buffer-local-value 'keymap-popup--prefix-mode buf)
              (setq prefix-arg '(4))))
        (funcall cmd)))))

(defun keymap-popup--submenu-overrides (submenu-pairs buf)
  "Return alist of submenu key overrides from SUBMENU-PAIRS for BUF."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (let ((target (cdr pair)))
                    (lambda () (interactive)
                      (keymap-popup--push-submenu buf target)))))
          submenu-pairs))

(defun keymap-popup--switch-overrides (keymap switch-keys buf)
  "Return alist of switch key overrides for KEYMAP's SWITCH-KEYS in BUF.
Wraps the toggle command with prefix-mode consumption."
  (mapcar (lambda (key-str)
            (cons key-str
                  (lambda () (interactive)
                    (call-interactively (keymap-lookup keymap key-str))
                    (when (buffer-local-value 'keymap-popup--prefix-mode buf)
                      (with-current-buffer buf
                        (setq-local keymap-popup--prefix-mode nil))
                      (setq prefix-arg nil))
                    (keymap-popup--refresh buf))))
          switch-keys))

(defun keymap-popup--stay-open-overrides (keymap stay-open-keys buf)
  "Return alist of stay-open suffix overrides for KEYMAP's STAY-OPEN-KEYS in BUF.
Each command executes and refreshes the popup in place."
  (mapcar (lambda (key-str)
            (cons key-str
                  (lambda () (interactive)
                    (call-interactively (keymap-lookup keymap key-str))
                    (keymap-popup--refresh buf))))
          stay-open-keys))

(defun keymap-popup--build-wrapper-map (keymap descriptions buf exit-key)
  "Build wrapper keymap over KEYMAP with DESCRIPTIONS for BUF.
EXIT-KEY and inapt guards are applied as a layer over specialized handlers."
  (let* ((map (make-sparse-keymap))
         (classified (keymap-popup--classify-entries descriptions))
         (inapt (plist-get classified :inapt))
         (overrides (append (keymap-popup--core-overrides exit-key)
                            (keymap-popup--switch-overrides
                             keymap (plist-get classified :switches) buf)
                            (keymap-popup--submenu-overrides
                             (plist-get classified :submenus) buf)
                            (keymap-popup--stay-open-overrides
                             keymap (plist-get classified :stay-open) buf))))
    (set-keymap-parent map keymap)
    (pcase-dolist (`(,key . ,cmd) overrides)
      (keymap-set map key
                  (if (member key inapt)
                      (keymap-popup--with-inapt-guard buf key cmd)
                    cmd)))
    ;; Inapt keys with no specialized handler get a guard over the base command
    (dolist (key inapt)
      (unless (assoc key overrides)
        (keymap-set map key
                    (keymap-popup--with-inapt-guard buf key
						    (lambda () (interactive)
						      (call-interactively (keymap-lookup keymap key)))))))
    map))

(defun keymap-popup-dismiss ()
  "Dismiss the active popup, if any.
Deactivates the transient map and removes the popup display."
  (when-let* ((buf (get-buffer "*keymap-popup*"))
              (map (buffer-local-value 'keymap-popup--wrapper-map buf)))
    (internal-pop-keymap map 'overriding-terminal-local-map)
    (keymap-popup--teardown buf)))

;;;###autoload
(defun keymap-popup (keymap)
  "Show popup help for described KEYMAP.
Activates KEYMAP as a transient map.  Switch keys execute and re-render
without closing.  Inapt keys are blocked.  Sub-menu keys push a
navigation stack.  \\[universal-argument] toggles prefix mode."
  (or (keymap-popup--meta keymap 'descriptions)
      (user-error "No descriptions in keymap"))
  (let* ((source (current-buffer))
         (buf (keymap-popup--prepare-buffer))
         (backend (funcall keymap-popup-backend))
         (descriptions (keymap-popup--resolve-descriptions
			(keymap-popup--collect-descriptions keymap)
			keymap))
         (docstring (keymap-popup--meta keymap 'description))
         (exit-key (or (keymap-popup--meta keymap 'exit-key)
                       keymap-popup-default-exit-key))
         (persistent (or (keymap-popup--meta keymap 'persistent)
                         keymap-popup-persistent)))
    (with-current-buffer buf
      (setq-local keymap-popup--source-buffer source
                  keymap-popup--active-keymap keymap
                  keymap-popup--active-descriptions descriptions
                  keymap-popup--active-docstring docstring
                  keymap-popup--active-exit-key exit-key
                  keymap-popup--persistent persistent
                  keymap-popup--display-backend backend
                  keymap-popup--stack nil
                  keymap-popup--prefix-mode nil
                  keymap-popup--reentering nil))
    (keymap-popup--refresh buf)
    (funcall (plist-get backend :show) buf)
    (let ((wrapper (keymap-popup--build-wrapper-map keymap descriptions buf exit-key)))
      (with-current-buffer buf
        (setq-local keymap-popup--wrapper-map wrapper))
      (set-transient-map wrapper
                         (keymap-popup--make-keep-pred buf)
                         (keymap-popup--make-on-exit buf)))
    (add-hook 'minibuffer-setup-hook #'keymap-popup--suspend)
    (add-hook 'minibuffer-exit-hook #'keymap-popup--resume)
    (when persistent
      (let ((hook-fn '#:keymap-popup--persistent-refresh))
        (fset hook-fn
              (lambda ()
                (if (buffer-live-p buf)
                    (keymap-popup--refresh buf)
                  (remove-hook 'post-command-hook hook-fn))))
        (add-hook 'post-command-hook hook-fn)))))

(provide 'keymap-popup)
;;; keymap-popup.el ends here
