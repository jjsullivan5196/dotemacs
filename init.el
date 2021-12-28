;;; init.el --- my init file -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; loads my packages
;;;
;;; Code:
;; * Environment
;; ** Basic procedures
(require 'seq)
(require 'subr-x)

(defalias '-> 'thread-first)
(defalias '->> 'thread-last)
(defalias 'do 'progn)

(defmacro comment (&rest _)
  "Ignore anything inside here."
  nil)

(defmacro doto (init &rest forms)
  "Evaluate INIT and thread it into FORMS, return the evaluated INIT."
  (let ((init-var (make-symbol "init-var")))
    `(let ((,init-var ,init))
       ,@(mapcar (lambda (form)
                   `(-> ,init-var ,form))
                 forms)
       ,init-var)))

(defun rotate-left (vs)
  "Rotate VS by one element."
  (append (cdr vs)
          (-> (car vs)
              list)))

(defun unbind-all (sym)
  "Remove all bindings from SYM."
  (fmakunbound sym)
  (makunbound sym))

(defun from-userdir (path)
  "Expand relative PATH from `user-emacs-directory`."
  (expand-file-name path user-emacs-directory))

(defvar cache-dir
  (let ((xdg-cache-home (getenv "XDG_CACHE_HOME")))
    (cond
     (xdg-cache-home (concat xdg-cache-home "/emacs"))
     (user-emacs-directory)))
  "The preferred location for temp files.")

;; Location of loadable config file.
(setq user-config-file (from-userdir "init.el"))

;; ** Loading packages
;; load modules from config dir
(add-to-list 'load-path (from-userdir "vendor/"))

;; package management details
(eval-when-compile
  (defvar bootstrap-version)

  (defun straight/bootstrap ()
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
      (load bootstrap-file nil 'nomessage)))

  (defun straight/use-package-handler-advice (handler name keyword args rest state)
    (if (equal "manual" (plist-get rest :pin))
        (use-package-process-keywords name rest state)
      (funcall handler name keyword args rest state)))

  (defun straight/install-use-package ()
    "Use straight to install all packages by default."
    (setq straight-use-package-by-default t)
    (straight-use-package 'use-package)
    (require 'use-package-ensure))

  (defun guix/install-use-package ()
    "Use guix to manage packages."
    (require 'use-package)
    (setq use-package-ensure-function #'ignore)
    (use-package guix
      :commands guix))

  ;; need to push customization after load, for most customizations to work
  (setq use-package-keywords '(:pin :ensure :disabled :load-path :requires :defines :functions :preface :if :when :unless :no-require :catch :after :bind :bind* :bind-keymap :bind-keymap* :interpreter :mode :magic :magic-fallback :hook :commands :init :defer :demand :load :config :custom :custom-face :diminish :delight))

  (straight/bootstrap)

  (require 'guix-emacs nil t)

  (if (featurep 'guix-emacs)
      (guix/install-use-package)
    (straight/install-use-package))

  (advice-add 'straight-use-package--straight-handler :around #'straight/use-package-handler-advice))

(when (featurep 'guix-emacs)
  (load "subdirs"))

;; ** Path
(use-package exec-path-from-shell
  :if (string-equal system-type "darwin")
  :config
  (exec-path-from-shell-initialize))

;; * The Journal
(comment

 ...)

;; * Helping hands
(defun run-command (command)
  "Run COMMAND in separate shell."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defmacro defcmd (name cmdline)
  "Define a function NAME to run CMDLINE with the shell."
  `(defun ,name ()
     (interactive)
     (run-command ,cmdline)))

(defun edit-user-config ()
  "Edit the main configuration file."
  (interactive)
  (find-file user-config-file))

(defun save-file (fname)
  "Save any change to file at path FNAME."
  (interactive)
  (let ((config (current-window-configuration)))
    (find-file fname)
    (save-buffer)
    (set-window-configuration config)))

(defun config-reinit ()
  "Reload init."
  (interactive)
  (doto user-config-file
    save-file
    load))

(defun bookmark-jump-nosave (bookmark)
  "Interactively jump to BOOKMARK without saving the window configuration."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark"
				                           bookmark-current-bookmark)))
  (bookmark-handle-bookmark bookmark))

(defun kill-this-buffer-now ()
  "Kill the active buffer."
  (interactive)
  (-> (window-buffer)
      kill-buffer))

(defun indent-buffer ()
  "Run `indent-region` on the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun juxt* (fns &rest args)
  "Apply list of functions FNS to ARGS, return a list of corresponding results."
  (mapcar (lambda (f) (apply f args)) fns))

(defun spit (obj fname)
  "Serialize OBJ and write it to file FNAME."
  (with-temp-buffer
    (print obj (current-buffer))
    (write-file fname)))

(defun read-first (fname)
  "Read first object from file FNAME."
  (with-temp-buffer
    (insert-file-contents fname)
    (read (current-buffer))))

(defun buffer-lines (buf)
  "Return all lines in BUF as a list."
  (save-excursion
    (with-current-buffer buf
      (end-of-buffer)
      (set 'buf-lines '()) ;;(list (thing-at-point 'line t)))
      (while (not (bobp))
        (forward-line -1)
        (set 'buf-lines (cons (thing-at-point 'line t) buf-lines)))
      buf-lines)))

;; * General customizations
;; truncate minibuffer display
(setq message-truncate-lines t)

;; y is y
(defalias 'yes-or-no-p 'y-or-n-p)

;; narrow?
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-list-file-prefix (concat cache-dir "/auto-save-list/.saves-"))
 '(backup-by-copying t)
 '(backup-directory-alist `((".*" \, (concat cache-dir "/backup"))))
 '(before-save-hook '(delete-trailing-whitespace))
 '(create-lockfiles nil)
 '(delete-old-versions t)
 '(fill-column 80)
 '(fringe-mode 1 nil (fringe))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "This aint your dad's spacemacs")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice (lambda nil (dired (getenv "HOME"))))
 '(initial-scratch-message ";; 頑張って!

")
 '(kept-new-versions 20)
 '(kept-old-versions 5)
 '(menu-bar-mode nil)
 '(python-indent-offset 2)
 '(ring-bell-function #'ignore)
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")))
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(version-control t))

;; colors!
(load-theme 'wombat t)

;; font size
(set-face-attribute 'default nil :height 120)

;; flat modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; * Keys
;; ** General
(use-package which-key
  :custom
  (which-key-mode t))

(use-package hydra)

(setq global-keys-mode-map (make-sparse-keymap))

(define-minor-mode global-keys-mode
  "Mode for global keybinds without messing with global keymap."
  :global t
  :keymap global-keys-mode-map)

(global-keys-mode 1)

(bind-keys
 :map bookmark-map
 ("B" . bookmark-jump-nosave))

;; ** Global binds
(bind-keys
 :map global-keys-mode-map
 ;; Ibuffer
 ([remap list-buffers] . ibuffer)

 ;; Font size
 ("C-=" . text-scale-increase)
 ("C--" . text-scale-decrease)
 ("C-+" . (lambda () (interactive) (text-scale-set 0)))

 ;; Config
 ("<f9>"  . config-reinit)
 ("<f12>" . edit-user-config))

;; ** Leader
;; *** General commands
(bind-keys
 :map global-keys-mode-map
 :prefix-map leader-command-map
 :prefix "M-SPC"
 ;; Exec commands
 ("<SPC>" . execute-extended-command))

;; Bookmarks
(bind-key "m" bookmark-map leader-command-map)

;; Help me
(bind-key "h" help-map leader-command-map)

;; *** Buffers
(bind-keys
 :map leader-command-map
 :prefix-map leader-buffers-map
 :prefix "b"
 ("f" . find-file)
 ("b" . switch-to-buffer)
 ("r" . revert-buffer)
 ("k" . kill-this-buffer-now)
 ("s" . save-buffer)
 ("w" . write-file))

;; ** Editing Shortcuts
(defhydra hydra-edit (global-keys-mode-map "<C-backspace>"
                                           :foreign-keys run)
  ("q" nil "quit")

  ;; Navigation
  ("n" consult-line)
  ("j" next-line)
  ("k" previous-line)
  ("h" backward-char)
  ("l" forward-char)
  ("e" forward-word)
  ("w" backward-word)
  ("]" forward-paragraph)
  ("[" backward-paragraph)
  ("H" move-beginning-of-line)
  ("L" move-end-of-line)

  ;; Editing
  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
  ("y" kill-ring-save)
  ("p" yank)
  ("d" kill-region)
  ("o" open-line)
  ("J" join-line)
  ("c" string-rectangle)
  ("C" comment-dwim)
  ("I" indent-buffer)

  ;; Selection
  ("v" set-mark-command)
  ("V" rectangle-mark-mode))

(defhydra hydra-sp-edit (global-keys-mode-map "<C-S-backspace>"
                                              :foreign-keys run)
  ("q" nil "quit")

  ;; Navigation
  ("n" consult-line)
  ("j" sp-forward-sexp)
  ("k" sp-backward-sexp)
  ("h" sp-up-sexp)
  ("l" sp-down-sexp)

  ;; Editing
  ("." sp-forward-slurp-sexp)
  ("," sp-forward-barf-sexp)
  ("J" sp-transpose-sexp)
  ("K" (sp-transpose-sexp -1))
  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
  ("y" kill-ring-save)
  ("p" yank)
  ("d" kill-region)
  ("c" string-rectangle)
  ("C" comment-dwim)
  ("I" indent-buffer)

  ;; Selection
  ("v" set-mark-command)
  ("V" rectangle-mark-mode))

;; * Utilities
;; ** Workspaces
(use-package tramp
  :pin manual
  :custom
  (tramp-persistency-file-name (concat cache-dir "/tramp")))

(use-package dired
  :pin manual
  :bind (:map dired-mode-map
              ("C-c C-o" . dired-open-at-point))
  :init
  (defun mime-open-file (path)
    "Open file at PATH with an appropriate application."
    (make-process :name "open-file"
                  :command (list "xdg-open" path)))

  (defun dired-open-at-point ()
    "Opens the file under the point with `mime-open-file`."
    (interactive)
    (-> (dired-filename-at-point)
        mime-open-file))
  :custom
  (dired-listing-switches "-alh"))

(use-package project)

(use-package projectile
  :bind (:map leader-buffers-map
              ("g" . projectile-find-file)))

(use-package magit
  :custom
  (project-switch-commands nil))

;; ** Shell
(use-package eshell
  :pin manual
  :init
  (defun eshell/clear ()
    "Actually clear eshell."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(use-package vterm
  :commands vterm
  :if (string-equal system-type "gnu/linux"))

;; ** Innernet
(use-package w3m
  :commands w3m)

(use-package web-search
  :disabled t
  :init (setq web-search-default-provider "DuckDuckGo")
  :bind ("C-c w" . web-search))

;; * Completion
(use-package vertico
  :custom
  (vertico-mode t))

(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("C-t" . marginalia-cycle))
  :custom
  (marginalia-mode t))

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package consult
  :bind (:map leader-command-map
              ("n" . consult-line)
         :map outline-minor-mode-map
              ("C-c s" . (lambda ()
                           (interactive)
                           (consult-outline)
                           (outline-hide-other)))
         :map global-keys-mode-map
              ("C-s" . consult-line))
  :custom
  (consult-project-root-function #'projectile-project-root))

(use-package embark
  :bind (:map minibuffer-local-map
              ("C-." . embark-act)))

(use-package embark-consult
  :pin manual
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package company
  :custom
  (global-company-mode t))

;; * Windows and frames
(use-package frames-only-mode
  :custom
  (frames-only-mode t))

;; ** Desktop
(use-package desktop
  :disabled t
  :custom
  (desktop-restore-frames nil)
  (desktop-save-mode t))

;; * Editing
(use-package undo-tree
  :custom
  (global-undo-tree-mode t))

(use-package wgrep)

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (from-userdir "snip")))
  (yas-global-mode t))

(use-package outshine
  :hook (prog-mode . outshine-mode))

(use-package org
  :pin manual
  :custom
  (org-babel-confirm-evaluate nil)
  (org-image-actual-width '(400)))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-hide-markup nil)
  (markdown-hide-urls t))

;; * Programming
;; ** General


(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package paren
  :pin manual
  :hook (prog-mode . show-paren-mode)
  :custom (show-paren-style 'expression)
  :config
  (set-face-background 'show-paren-match "#432121"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind (:map prog-mode-map
              ("C-M-k" . sp-forward-slurp-sexp)
              ("C-s-k" . sp-forward-barf-sexp)
              ("C-M-j" . sp-backward-slurp-sexp)
              ("C-s-j" . sp-backward-barf-sexp)))

(use-package flycheck
  :custom
  (flycheck-display-errors-function nil)
  (global-flycheck-mode t))

(use-package flymake
  :pin manual)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(typescript-mode . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    '(:enable t
      :lint t)))

(use-package expand-region
  :bind (:map leader-command-map
              ("." . 'er/expand-region)))

(use-package nix-mode
  :mode "\\.nix\\'")

;; ** Lisp
(use-package geiser-guile)

(use-package geiser)

(use-package racket-mode
  :bind (:map racket-mode-map
              ("C-x C-e" . racket-eval-last-sexp)))

(use-package flycheck-clj-kondo
  :straight t
  :after (flycheck))

(use-package cider
  :init
  (require 'ob-clojure)
  (defun cider-prestart ()
    "Set local bindings for cider."
    (add-hook 'before-save-hook #'cider-format-buffer t t))

  (defun cider-kill-stdout-eval-handler ()
    "Eval handler that sends any output to the kill ring."
    (nrepl-make-response-handler
     (current-buffer)
     ;; value
     nil
     ;; output
     (lambda (_buf out)
       (with-current-buffer (get-buffer-create "*cider-kill-scratch*")
         (insert (format "%s" out))))
     ;; error
     nil
     ;; done
     (lambda (_buf)
       (with-current-buffer (get-buffer-create "*cider-kill-scratch*")
         (kill-ring-save (point-min) (point-max))))))

  (defun cider-eval-defun-output-to-kill-ring ()
    "Evaluate defun at point, send all standard out to kill ring."
    (interactive)
    (let* ((bounds (cider-defun-at-point t)))
      (with-current-buffer (get-buffer-create "*cider-kill-scratch*")
        (delete-region (point-min) (point-max)))
      (cider-interactive-eval nil (apply-partially
                                   'juxt* (list
                                           (cider-kill-stdout-eval-handler)
                                           (cider-interactive-eval-handler (current-buffer) bounds)))
                              bounds)))

  :hook (clojure-mode . cider-prestart)
  :bind (:map cider-mode-map
              ("C-, e" . 'cider-eval-commands-map)
              :map cider-eval-commands-map
              ("k" . 'cider-eval-defun-output-to-kill-ring))
  :custom
  (cider-print-fn 'fipp)
  (clojure-indent-style 'align-arguments)
  (org-babel-clojure-backend 'cider)
  (clojure-toplevel-inside-comment-form t)
  (cider-repl-pop-to-buffer-on-connect nil)

  ;; Format pretty-printed comments to appear as evaluation output
  (cider-comment-prefix "\n  #_=> ")
  (cider-comment-continued-prefix "       ")
  (cider-comment-postfix "\n")

  (cider-clojure-cli-aliases "dev")
  (cider-shadow-cljs-command "clojure -M:shadow-cljs"))

;; ** Javascript
(use-package js
  :pin manual
  :custom
  (js-indent-level 2))

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package svelte-mode
  :disabled t)

(use-package php-mode)
(use-package web-mode)

(use-package restclient)

;; ** Gophers
(use-package go-mode)

;; ** PowerHell
(use-package powershell-mode
  :straight t)

;; ** Lua
(use-package lua-mode
  :custom
  (lua-indent-level 2))

;; * End
(provide 'init)
;;; init.el ends here
