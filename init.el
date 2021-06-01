;; -*- eval: (outshine-mode 1); -*-

;;; Prelude
;; load modules from config dir
(add-to-list 'load-path
             (expand-file-name "vendor/" user-emacs-directory))

;; enable package manager
(require 'straight-init)

;; dash!
(use-package dash)

;;; Helping hands
(defun from-userdir (path)
  "Expand relative PATH from `user-emacs-directory`."
  (expand-file-name path user-emacs-directory))

(defun mime-open-file (path)
  "Open file at PATH with an appropriate application."
  (make-process :name "open-file"
                :command (list "xdg-open" path)))

(defun dired-open-at-point ()
  "Opens the file under the point with `mime-open-file`."
  (interactive)
  (-> (dired-filename-at-point)
      mime-open-file))

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
  (-doto user-config-file
    (save-file)
    (load)))

(defun kill-this-buffer-now ()
  "Kill the active buffer."
  (interactive)
  (-> (window-buffer)
      kill-buffer))

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

(defmacro comment (&rest _)
  "Ignore anything inside here."
  nil)

(defun unbind-all (sym)
  "Remove all bindings from SYM."
  (fmakunbound sym)
  (makunbound sym))

(defalias 'do 'progn)

;;; Configure default behavior
;; Set config sources
(defvar user-config-file
  (from-userdir "init.el")
  "Location of loadable config file.")

;; ahhhhhhhhhhhhh
(setq ring-bell-function 'ignore)

;; y is y
(defalias 'yes-or-no-p 'y-or-n-p)

;; backups
(setq backup-directory-alist `((".*" . ,(from-userdir "backup")))
      auto-save-default nil
      create-lockfiles nil
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; colors!
(load-theme 'wombat)

;; Shrink fringes to 1 pixel.
(fringe-mode 1)

;; font size
(set-face-attribute 'default nil :height 120)

;; flat modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; turn off stupid bars (in order of greatest annoyance)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; truncate minibuffer display
(setq message-truncate-lines t)

;; trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; prog-mode settings
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; line flow
(setq-default fill-column 80)

;; narrow?
(put 'narrow-to-region 'disabled nil)

;; messin with windows
(winner-mode 1)

;; self-care
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "This aint your dad's spacemacs")
(setq initial-scratch-message ";; 頑張って!\n\n")

;; org-mode
(require 'org)
(setq-default org-confirm-babel-evaluate nil)
(setq org-image-actual-width '(400))

;;; Editor stuff
(use-package outshine)

(use-package exec-path-from-shell
  :config
  (when (string-equal system-type "darwin")
    (exec-path-from-shell-initialize)))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.config/snip"))
  (yas-global-mode 1))

(use-package ace-window
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit    ace-jump-face-foreground
          :height     4.5
          :foreground "red"))))))

(use-package frames-only-mode
  :disabled t
  :config (frames-only-mode 1))

(use-package which-key
  :config (which-key-mode 1))

(use-package undo-tree
  :config (global-undo-tree-mode 1))

(use-package hydra)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :after (ivy)
  :config
  (counsel-mode 1))

(use-package swiper
  :after (ivy)
  :config
  (global-set-key [remap isearch-forward] 'swiper-isearch)
  (global-set-key [remap isearch-backward] 'swiper-isearch-backward))

(setq ivy-initial-inputs-alist nil)

(use-package company
  :config (global-company-mode 1))

(use-package sunrise
  :straight (sunrise :type git
                     :host github
                     :repo "sunrise-commander/sunrise-commander"))
;;; Perspectives
(defvar perspectives-table
  (make-hash-table :test 'equal)
  "Holds onto saved window states to be used again later.")

(comment
 (unbind-all 'perspectives-table))

;;;; Serialization
(defvar perspectives-file
  (from-userdir "perspectives.dat")
  "Location to serialize perspectives.")

(defun save-perspectives (&optional fname)
  "Save open perspectives to file FNAME."
  (interactive)
  (let ((fname (or fname perspectives-file)))
    (spit perspectives-table fname)))

(defun load-perspectives (&optional fname)
  "Load perspectives from file FNAME."
  (interactive)
  (let ((fname (or fname perspectives-file)))
    (when (file-exists-p fname)
      (setq perspectives-table
            (or (read-first fname)
                perspectives-table)))))

(add-hook 'kill-emacs-hook 'save-perspectives)
(add-hook 'emacs-startup-hook 'load-perspectives)

;;;; Interaction
(defun choose-perspective (prompt)
  "Choose an open perspective with PROMPT."
  (ivy-read prompt perspectives-table))

(defun add-perspective (name window-state)
  "Add perspective NAME with WINDOW-STATE to perspectives."
  (interactive (list (choose-perspective "Add perspective: ")
                     (window-state-get (frame-root-window) t)))
  (puthash name window-state perspectives-table))

(defun switch-perspective (name)
  "Jump to NAME perspective."
  (interactive (list (choose-perspective "Switch to: ")))
  (let ((window-state (gethash name perspectives-table)))
    (when window-state
      (window-state-put window-state))))

(defun remove-perspective (name)
  "Remove NAME perspective."
  (interactive (list (choose-perspective "Remove: ")))
  (remhash name perspectives-table))

;;; Desktop
(setq desktop-restore-frames nil)
(desktop-save-mode 1)

;;; Markup
(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq-default markdown-hide-markup t
                markdown-hide-urls t))

;;; Eshell
(require 'eshell)

(defun eshell/clear ()
  "Actually clear eshell."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;; Innernet
(use-package w3m
  :disabled)

(use-package web-search
  :init (setq web-search-default-provider "DuckDuckGo")
  :bind ("C-c w" . web-search))

;;; Keys
(setq global-keys-mode-map (make-sparse-keymap))

(define-minor-mode global-keys-mode
  "Mode for global keybinds without messing with global keymap."
  :global t
  :keymap global-keys-mode-map)

(global-keys-mode 1)

(bind-keys
 :map dired-mode-map
 ("C-c C-o" . dired-open-at-point))

;;;; Global binds
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

;;;; Leader
;; General commands
(bind-keys
 :map global-keys-mode-map
 :prefix-map leader-command-map
 :prefix "M-SPC"
 ;; Exec commands
 ("<SPC>" . counsel-M-x)

 ;; Interactive search
 ("n" . swiper-isearch)
 ("N" . swiper-isearch-backward))

;; Help me
(bind-key "h" help-map leader-command-map)

;; Buffers
(bind-keys
 :map leader-command-map
 :prefix-map leader-buffers-map
 :prefix "b"
 ("f" . find-file)
 ("g" . counsel-git)
 ("b" . switch-to-buffer)
 ("r" . revert-buffer)
 ("k" . kill-this-buffer-now)
 ("s" . save-buffer)
 ("w" . write-file))

;; Frames
(bind-keys
 :map leader-command-map
 :prefix-map leader-frames-map
 :prefix "f"
 ("f" . make-frame-command)
 ("k" . delete-frame)
 ("c" . remove-perspective))

(defhydra hydra-frame-windows (leader-command-map "w")
  ("q" nil "quit")
  ("j" shrink-window "shrink length")
  ("k" enlarge-window "expand length")
  ("h" shrink-window-horizontally "shrink width")
  ("l" enlarge-window-horizontally "expand width")
  ("a" ace-window "change window")
  ("A" (lambda () (interactive) (ace-window 4)) "swap windows")
  ("d" delete-window "delete window")
  ("r" delete-other-windows "remove others")
  ("b" split-window-right "split vertically")
  ("B" split-window-below "split horizontally")
  ("u" winner-undo "undo change")
  ("U" winner-redo "redo change")
  ("v" switch-perspective "change perspective")
  ("V" add-perspective "save perspective"))

;;; Editing Shortcuts
(defhydra hydra-edit (global-keys-mode-map "<C-backspace>")
  ("q" nil "quit")

  ;; Navigation
  ("n" swiper-isearch)
  ("N" swiper-isearch-backward)
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
  ("d" delete-region)
  ("o" open-line)
  ("J" join-line)
  ("c" string-rectangle)

  ;; Selection
  ("v" set-mark-command)
  ("V" rectangle-mark-mode))

(defhydra hydra-sp-edit (global-keys-mode-map "<C-S-backspace>")
  ("q" nil "quit")

  ;; Navigation
  ("n" swiper-isearch)
  ("N" swiper-isearch-backward)
  ("j" sp-forward-sexp)
  ("k" sp-backward-sexp)
  ("h" sp-up-sexp)
  ("l" sp-down-sexp)
  ("]" forward-paragraph)
  ("[" backward-paragraph)

  ;; Editing
  ("." sp-forward-slurp-sexp)
  ("," sp-forward-barf-sexp)
  ("w" sp-transpose-sexp)
  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
  ("y" kill-ring-save)
  ("p" yank)
  ("d" delete-region)
  ("c" string-rectangle)

  ;; Selection
  ("v" set-mark-command)
  ("V" rectangle-mark-mode))

;;; Programming
;;;; General
(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2
              sh-basic-offset 2)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :hook ((prog-mode . smartparens-mode))
  :bind
  (:map prog-mode-map
        ("C-M-k" . sp-forward-slurp-sexp)
        ("C-s-k" . sp-forward-barf-sexp)
        ("C-M-j" . sp-backward-slurp-sexp)
        ("C-s-j" . sp-backward-barf-sexp)))

(show-paren-mode t)
(setq show-paren-style 'expression)

;;;; IDE things
(use-package project-el
  :disabled
  :straight (project-el :type git
                        :host github
                        :repo "jorgenschaefer/project-el"
                        :files ("project.el")))

(use-package eglot
  :after (project-el))

(use-package flycheck
  :config (global-flycheck-mode 1))

(use-package expand-region
  :bind (:map leader-command-map
              ("." . 'er/expand-region)))

(use-package projectile)

(use-package magit
  :config (setq project-switch-commands nil))

(use-package direnv
  :config (direnv-mode))

(use-package nix-mode
  :mode "\\.nix\\'")


;;;; C/C++
;;(use-package helm-gtags)

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

(defun add-gtags-hook ()
  (add-hook 'after-save-hook #'gtags-update-hook 0 t))

;;(add-hook 'c-mode-hook #'add-gtags-hook)

;;;; Lisp
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

(use-package flycheck-clj-kondo
  :after (flycheck cider))

(use-package cider
  :init
  (setq-default clojure-indent-style 'align-arguments)
  (setq org-babel-clojure-backend 'cider)
  (require 'ob-clojure)
  :hook (clojure-mode . cider-prestart)
  :bind (:map cider-mode-map
              ("C-, e" . 'cider-eval-commands-map)
         :map cider-eval-commands-map
              ("k" . 'cider-eval-defun-output-to-kill-ring))
  :config
  (setq cider-print-fn 'fipp)
  (setq clojure-toplevel-inside-comment-form t)

  ;; Format pretty-printed comments to appear as evaluation output
  (setq cider-comment-prefix "\n  #_=> ")
  (setq cider-comment-continued-prefix "       ")
  (setq cider-comment-postfix "\n")

  (setq cider-clojure-cli-aliases "dev")
  (setq cider-shadow-cljs-command "clojure -A:shadow-cljs")
  (require 'flycheck-clj-kondo))

;;;; Javascript
(setq js-indent-level 2
      js-switch-indent-offset 2)

(use-package rjsx-mode
  :mode (("\\.js\\'"  . rjsx-mode)
         ("\\.ts\\'"  . rjsx-mode)
         ("\\.tsx\\'" . rjsx-mode))
  :config (setq js2-strict-missing-semi-warning nil))

(use-package coffee-mode)

(use-package svelte-mode)

(use-package qml-mode)

(use-package haxe-mode)

(use-package vue-mode)

(use-package tide
  :after (rjsx-mode flycheck)
  :hook ((rjsx-mode . tide-setup)
         (rjsx-mode . tide-hl-identifier-mode)))

(use-package php-mode)
(use-package web-mode)

;;;; Snake
(setq python-indent-offset 2)

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

;;;; Gophers
(use-package go-mode)

;;;; Rust
(use-package rust-mode)

;;;; Zig
(use-package zig-mode
  :after (eglot)
  :config
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls")))
  (add-hook 'zig-mode-hook 'eglot-ensure))
