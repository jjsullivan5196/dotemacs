;;; setup-init.el --- bootstrap for the setup macro -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Setup the package manager and get the `setup' macro ready for use.
;;;
;;; Code:
(require 'straight-init)
(require 'guix-emacs-init)
(ensure-package 'setup)

(require 'setup)

(setup-define :packages
  (lambda (&rest pkgs)
    (let ((installed-packages (intern (format "%s-packages" (setup-get 'feature)))))
      `(progn
         (defvar ,installed-packages nil
           "List of packages declared by `:packages'.")

         (setq ,installed-packages ',pkgs)
         (ensure-packages ,installed-packages))))
  :documentation "Ensure packages PKGS are ready to be used.")

(defun set-both (name x)
  "Bind NAME to X in both namespaces."
  (set name x)
  (fset name x))

(setup-define :keymap
  (lambda (name)
    `(set-both ',name (make-sparse-keymap)))
  :documentation "Set NAME to a new keymap."
  :repeatable t)

(setup-define :minor-mode
  (lambda (name &rest minor-mode-opts)
    (let ((mode-map (intern (concat (symbol-name name) "-map"))))
	    `(progn
	       (set-both ',mode-map (make-sparse-keymap))
	       (define-minor-mode ,name
	         ,@minor-mode-opts
	         :keymap ,mode-map)
	       (provide ',name))))
  :documentation "Create a minor mode NAME, with a corresponding keymap.

MINOR-MODE-OPTS will be passed to `define-minor-mode'. This macro
can be used as NAME, and will replace itself with the name of the
new minor mode."
  :shorthand #'cadr
  :indent 1)

(setup-define :hydra
  (lambda (name opts &rest defs)
    `(defhydra ,name (,(setup-get 'map) ,@opts)
       ,@defs))
  :documentation "Bind a new hydra using the local keymap."
  :indent 2)

(setup-define :use-global-mode
  (lambda (name)
    (let ((mode (if (string-match-p "-mode\\'" (symbol-name name))
                    name
                  (intern (format "%s-mode" name)))))
      `(,mode t)))
  :documentation "Activate a global minor mode by NAME.

This macro can be used as NAME, and will replace itself with the
package name of the first minor mode."
  :repeatable t
  :shorthand #'cadr)

(provide 'setup-init)
;;; setup-init.el ends here
