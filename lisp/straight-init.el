;;; straight-init.el --- bootstrap for straight package manager -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Includes straight's bootstrap, and provides a front for managing packages in
;;; aggregate.
;;;
;;; Code:
(require 'seq)

(defvar bootstrap-version)

(let ((bootstrap-file
	     (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	    (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
	    (goto-char (point-max))
	    (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun straight-update-packages (pkgs)
  "Sync straight with the PKGS list and autoload all packages."
  (seq-do #'straight-use-package pkgs)
  t)

(defalias 'ensure-package #'straight-use-package)
(defalias 'ensure-packages #'straight-update-packages)

(provide 'straight-init)
;;; straight-init.el ends here
