;;
;; To use this file, just put a (require 'tempo-c-cpp) in your .emacs file 
;;
;; Note on tempo (from EmacsWiki): 
;; templates are defined through tempo-define-template. they uses (p ...) to prompt for variables 
;; and (s ...) to insert them again. > indents, n inserts a newline, and r inserts the region, if active.
;;
;; To use the templates defined here: 
;; - either run M-x tempo-template-c-<xx> where <xx> is the name of the template (use TAB to have the list)
;; - or start to type the corresponding abbreviation (list follows) and hit C-RET or F5
;;
;; Feel free to adapt the templates to your own programming style.
;;
;; List of abbreviations: 
;;  	<abbrev>		<correspondant sequence> 
;; ---- Preprocessor statements --- 
;;    	include          	#include   
;;   	define          	#define
;;    	ifdef           	#ifdef
;;    	ifndef          	#ifndef 
;; --- C statements
;;    	if            		if (...) { }
;;    	else  			else { ... }
;;    	ifelse 			if (...) { } else { }
;;    	while			while (...) { }
;;    	for			for (...) { }
;;    	fori			for (i=0; i < limit; i++) { }
;;    	switch			switch() {...}
;;    	case			case: ... break;
;;    	main			int main() { ... }
;;    	malloc			type * var = (type *) malloc(...)
;; --- C++ statements
;;    class			class xxx { ... };
;;    getset			accessor/mutator

(require 'tempo)
(setq tempo-interactive t)

(defvar c-tempo-tags nil
  "Tempo tags for C mode")

(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(defvar c-tempo-keys-alist nil
  "")

(defun my-tempo-c-cpp-bindings ()
  ;;(local-set-key (read-kbd-macro "<f8>") 'tempo-forward-mark)
  (local-set-key (read-kbd-macro "M-<return>")   'tempo-complete-tag)
  ;;(local-set-key (read-kbd-macro "<f5>")   'tempo-complete-tag)
  (tempo-use-tag-list 'c-tempo-tags)
  (tempo-use-tag-list 'c++-tempo-tags))

(add-hook 'c-mode-hook   '(lambda () (my-tempo-c-cpp-bindings)))
(add-hook 'c++-mode-hook '(lambda () (my-tempo-c-cpp-bindings)))

;; the following macros allow to set point using the ~ character in tempo templates
 
(defvar tempo-initial-pos nil
   "Initial position in template after expansion")
 (defadvice tempo-insert( around tempo-insert-pos act )
   "Define initial position."
   (if (eq element '~)
         (setq tempo-initial-pos (point-marker))
     ad-do-it))
 (defadvice tempo-insert-template( around tempo-insert-template-pos act )
   "Set initial position when defined. ChristophConrad"
   (setq tempo-initial-pos nil)
   ad-do-it
   (if tempo-initial-pos
       (progn
         (put template 'no-self-insert t)
         (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

;;; Preprocessor Templates (appended to c-tempo-tags)
(tempo-define-template "c-include"
		       '("#include <" r ".h>" > n
			 )
		       "include"
		       "Insert a #include <> statement"
		       'c-tempo-tags)

(tempo-define-template "c-define"
		       '("#define " r " " > n
			 )
		       "define"
		       "Insert a #define statement"
		       'c-tempo-tags)

(tempo-define-template "c-ifdef"
		       '("#ifdef " (p "ifdef-condition: " clause) > n> p n
			 "#else /* !(" (s clause) ") */" n> p n
			 "#endif // " (s clause) n>
			 )
		       "ifdef"
		       "Insert a #ifdef #else #endif statement"
		       'c-tempo-tags)

(tempo-define-template "c-ifndef"
		       '("#ifndef " (p "ifndef-clause: " clause) > n 
			 "#define " (s clause) n> p n
			 "#endif // " (s clause) n>
			 )
		       "ifndef"
		       "Insert a #ifndef #define #endif statement"
		       'c-tempo-tags)

;;; C-Mode Templates
(tempo-define-template "c-if"
		       '(> "if (" ~ " ) { "  n>
			 > n
			 "}" > n> 
			 )
		       "if"
		       "Insert a C if statement"
		       'c-tempo-tags)

(tempo-define-template "c-else"
		       '(> "else {" n>
			 > ~ n 
			 "}" > n>
			 )
		       "else"
		       "Insert a C else statement"
		       'c-tempo-tags)

(tempo-define-template "c-if-else"
                       '(> "if (" ~ " ) { "  n>
                         > n
                         "} else {" > n>
			 > n
			 "}" > n>     
			 )
		       "ifelse"
		       "Insert a C if else statement"
		       'c-tempo-tags)

(tempo-define-template "c-while"
                       '(> "while (" ~ " ) { "  n>
                         > n
                         "}" > n>      
                         )
		       "while"
		       "Insert a C while statement"
		       'c-tempo-tags)

(tempo-define-template "c-for"
                       '(> "for (" ~ " ) { "  n>
                         > n
                         "}" > n>      
                         )
		       "for"
		       "Insert a C for statement"
		       'c-tempo-tags)

(tempo-define-template "c-for-i"
		       '(> "for (" (p "variable: " var) " = 0; " (s var)
			 " < "(p "upper bound: " ub)"; " (s var) "++) {" >  n> 
			 > r n 
			 "}" > n>
			 )
		       "fori"
		       "Insert a C for loop: for(x = 0; x < ..; x++)"
		       'c-tempo-tags)

(tempo-define-template "c-malloc"
		       '(>(p "type: " type) " * " (p "variable name: " var) " = (" (s type) " *) malloc(sizeof(" (s type) "));" n>
			  "if (" (s var) " == NULL) {" n>
			  > r n
			 "}" > n>
			 )
		       "malloc"
		       "Insert a C malloc statement to define and allocate a pointer"
		       'c-tempo-tags)

(tempo-define-template "c-main"
		       '(> "int main(int argc, char *argv[]) {" >  n> 
			 > r n
			 "return 0;" > n
			 "}" > n>
			 )
		       "main"
		       "Insert a C main statement"
		       'c-tempo-tags)

(tempo-define-template "c-switch"
		       '(> "switch(" (p "variable to check: " clause) ") {" >  n>  
			 "case " > (p "first value: ") ": " ~ > n>
			 " break;" > n>
			 >"default:" > n>
			 "}" > n>
			 )
		       "switch"
		       "Insert a C switch statement"
		       'c-tempo-tags)

(tempo-define-template "c-case"
		       '("case " (p "value: ") ":" ~ > n>
			   "break;" > n>
			)
		       "case"
		       "Insert a C case statement"
		       'c-tempo-tags)

;;;C++-Mode Templates
;;(setq max-lisp-eval-depth 500) 

(tempo-define-template "c++-class"
		        '("/**" > n> 
			  "*" (s class) "."> n>
			  "*/" > n>
			  "class " (p "classname: " class) " {" > n> 
			  "friend std::ostream& operator<<(std::ostream& os, const " (s class) " & f);" > n> 
			  > n>
			  (p "variable member type: " type 'noinsert)
                          (p "variable member name: " var  'noinsert)
			  (tempo-save-named 'm_var (concat "_" (tempo-lookup-named 'var)))
			  (s type) " " (s m_var) ";" > n>
			  > ~ n>	
			  "public:" > n>
			  (s class) "(); \t//the default constructor" n>
			  (s class) "(const " (s class) " &c);" n>
			  "~" (s class) "() {}" > n>
			  > n>
			  "/* Accessors */" > n>
			  (s type) " get" (s fnBase) "() const { return "(s m_var) "; }" > n>
			  > n>
			  "/* Mutators */" > n>
			  "void set" (s fnBase) "(" (s type) " " (s var) ") { " (s m_var) " = " (s var) "; }" > n>
			  > n>
			 "};\t// end of class " (s class) > n>
			 )
		       "class"
		       "Insert a class skeleton"
		       'c++-tempo-tags)

(tempo-define-template "c++-getset"
		       '((p "type: "     type 'noinsert)
			 (p "variable: " var  'noinsert)
			 (tempo-save-named 'virtual (if (y-or-n-p  "virtual?") "virtual " ""))
			 (tempo-save-named 'm_var (concat "_" (tempo-lookup-named 'var)))
			 (tempo-save-named 'fnBase (upcase-initials (tempo-lookup-named 'var)))
			 (s type) " " (s m_var) ";" > n>
			 (s virtual) (s type) " get" (s fnBase) "() const { return "(s m_var) "; }" > n>
			 (s virtual) "void set" (s fnBase) "(" (s type) " " (s var) ") { " (s m_var) " = " (s var) "; }" > n>
			 )
		       "getset"
		       "Insert get set methods"
		       'c++-tempo-tags)
(tempo-define-template "c++-nsamespace-std"
		       '("using namespace std;" > n
			 )
		       "std"
		       "Insert a using namespace std statement"
		       'c++-tempo-tags)


(provide 'tempo-c-cpp)
;;; tempo-c-cpp.el ends here

