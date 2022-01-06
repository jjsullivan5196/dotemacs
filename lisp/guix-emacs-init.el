;;; guix-emacs-init.el --- manage emacs packages with guix -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; This module builds on guix's base capabilities, in order to manage packages
;;; interactively.
;;;
;;; Code:
(defvar guix-emacs-init-profile
  (expand-file-name "init-package-profile" user-emacs-directory)
  "Profile used as the default to install Emacs packages.")

(when (require 'guix-repl nil t)
  (require 'seq)

  (defun guix-emacs-profile-packages (profile)
    "Get the Emacs packages installed in PROFILE."
    (let* ((script `(begin
                     (use-modules (guix profiles))
                     (manifest->code (profile-manifest ,profile))))
           ;; god why
           (manifest (cdr (cadadr (guix-eval-read (prin1-to-string script)))))
           (specs (seq-filter (apply-partially #'string-match "^emacs-.*")
                              manifest))
           (pkgs (mapcar (lambda (s)
                           (intern (seq-subseq s 6)))
                         specs)))
      pkgs))

  (defun guix-emacs-add-profile (profile)
    "Add PROFILE to the load path."
    (let ((site-lisp (expand-file-name "share/emacs/site-lisp" profile)))
      (add-to-list 'load-path site-lisp)
      (load (expand-file-name "subdirs" site-lisp))))

  (defun guix-emacs-install-package (profile pkg)
    "Add a single package PKG to PROFILE."
    (let ((script `(guix-command "package"
                                 "-p" ,profile
                                 "-i" ,(format "emacs-%s" pkg))))
      (guix-eval-read (prin1-to-string script))))

  (defun guix-emacs-build-profile (profile pkgs)
    "Update PROFILE with the packages in PKGS, return when the new profile is ready."
    (let* ((specs (mapcar (apply-partially #'format "emacs-%s") pkgs))
           (script `(begin
                     (use-modules (guix store)
                                  (guix scripts package)
                                  (gnu packages))
                     (with-profile-lock
                      ,profile
                      (build-and-use-profile
                       (open-connection)
                       ,profile
                       (specifications->manifest (list "pgtk-emacs-next" ,@specs)))))))
      (guix-eval-read (prin1-to-string script))))

  (defun guix-emacs-sync-packages (profile pkgs)
    "Sync PROFILE and autoload all PKGS."
    (guix-emacs-build-profile profile pkgs)
    (guix-emacs-add-profile profile)
    (guix-emacs-autoload-packages))

  (defun ensure-package (pkg)
    "Ensure PKG is installed and autoloaded."
    (when (not (memq pkg (guix-emacs-profile-packages guix-emacs-init-profile)))
      (guix-emacs-install-package guix-emacs-init-profile pkg)
      (guix-emacs-add-profile guix-emacs-init-profile))
    (load (format "%s-autoloads.el" pkg) t)
    t)

  (defun ensure-packages (pkgs)
    "Ensure PKGS are installed and autoloaded."
    (when (not (equal pkgs (guix-emacs-profile-packages guix-emacs-init-profile)))
      (guix-emacs-sync-packages guix-emacs-init-profile pkgs))
    t))

(provide 'guix-emacs-init)
;;; guix-emacs-init.el ends here
