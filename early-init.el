;;; early-init.el --- modify Emacs startup -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Change any default startup behavior here.
;;;
;;; Code:

;; * Disable package.el
(setq package-enable-at-startup nil)

;; * Load user elisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; * End
(provide 'early-init)
;;; early-init.el ends here
