(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  )

(use-package bazel
  :mode (("\\.bazel\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode)
         ("\\BUILD\\'" . bazel-mode)
         ("\\WORKSPACE\\'" . bazel-mode))
  )
