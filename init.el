;; add user modules to load path
(add-to-list 'load-path (expand-file-name "vendor"
                                          user-emacs-directory))

;; Set config sources
(defvar user-config-file (expand-file-name "vendor/user-config.el"
                                           user-emacs-directory)
  "Location of loadable config file.")

(defvar user-config-source-file (expand-file-name "vendor/user-config.org"
                                                  user-emacs-directory)
  "Source document for config file.")

;; Compile config if newer or non-existent
(when (file-newer-than-file-p user-config-source-file
                              user-config-file)
  (require 'ob-tangle)
  (org-babel-tangle-file user-config-source-file
                         user-config-file
                         "emacs-lisp"))

;; Load config
(load user-config-file)
