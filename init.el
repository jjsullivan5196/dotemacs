;; -*- eval: (outshine-mode 1); -*-

;;; Prelude
;; load modules from config dir
(add-to-list 'load-path
             (expand-file-name "vendor/" user-emacs-directory))

;; enable straight when guix isn't available
;; if we do have guix, refresh the load path for new packages
(let ((guix-profile (getenv "GUIX_PROFILE")))
  (if (not (getenv "GUIX_PROFILE"))
      (require 'straight-init)
    (progn
      (load "subdirs")
      (require 'use-package))))

;; dash!
(use-package dash)

(require 'seq)
(require 'subr-x)

;;; Helping hands
(defun from-userdir (path)
  "Expand relative PATH from `user-emacs-directory`."
  (expand-file-name path user-emacs-directory))

(defun run-command (command)
  "Run COMMAND in separate shell."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defmacro defcmd (name cmdline)
  "Define a function NAME to run CMDLINE with the shell."
  `(defun ,name ()
     (interactive)
     (run-command ,cmdline)))

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

(defmacro comment (&rest _)
  "Ignore anything inside here."
  nil)

(defun unbind-all (sym)
  "Remove all bindings from SYM."
  (fmakunbound sym)
  (makunbound sym))

(defalias 'do 'progn)

(defcmd net-settings
  "cmst -Md")

;;; The Journal
(comment

 '...)
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

;;; Keys
;;;; General
(use-package hydra)

(setq global-keys-mode-map (make-sparse-keymap))

(define-minor-mode global-keys-mode
  "Mode for global keybinds without messing with global keymap."
  :global t
  :keymap global-keys-mode-map)

(global-keys-mode 1)

(require 'dired)

(bind-keys
 :map dired-mode-map
 ("C-c C-o" . dired-open-at-point))

(bind-keys
 :map bookmark-map
 ("B" . bookmark-jump-nosave))

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
;;;;; General commands
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

;;;;; Buffers
(bind-keys
 :map leader-command-map
 :prefix-map leader-buffers-map
 :prefix "b"
 ("f" . find-file)
 ("g" . projectile-find-file)
 ("b" . switch-to-buffer)
 ("r" . revert-buffer)
 ("k" . kill-this-buffer-now)
 ("s" . save-buffer)
 ("w" . write-file))

;;;;; Frames
(bind-keys
 :map leader-command-map
 :prefix-map leader-frames-map
 :prefix "f"
 ("f" . make-frame-command)
 ("k" . delete-frame))

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
  ("U" winner-redo "redo change"))

;;; Editing Shortcuts
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

;;; Editor stuff
(use-package which-key
  :config
  (which-key-mode 1))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package consult
  :bind (:map leader-command-map
              ("n" . consult-line)
         :map global-keys-mode-map
              ("C-s" . consult-line)
              ;; Custom M-# bindings for fast register access
              ;;("M-#"      . consult-register-load)
              ;;("M-'"      . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
              ;;("C-M-#"    . consult-register)
              ;; Other custom bindings
              ;;("M-y"      . consult-yank-pop)                ;; orig. yank-pop
              ;;("<help> a" . consult-apropos)            ;; orig. apropos-command
              ;; M-g bindings (goto-map)
              ;;("M-g e"    . consult-compile-error)
              ;;("M-g f"    . consult-flymake)               ;; Alternative: consult-flycheck
              ;;("M-g g"    . consult-goto-line)             ;; orig. goto-line
              ;;("M-g M-g"  . consult-goto-line)           ;; orig. goto-line
              ;;("M-g o"    . consult-outline)               ;; Alternative: consult-org-heading
              ;;("M-g m"    . consult-mark)
              ;;("M-g k"    . consult-global-mark)
              ;;("M-g i"    . consult-imenu)
              ;;("M-g I"    . consult-project-imenu)
              ;; M-s bindings (search-map)
              ;;("M-s f"    . consult-find)
              ;;("M-s L"    . consult-locate)
              ;;("M-s g"    . consult-grep)
              ;;("M-s G"    . consult-git-grep)
              ;;("M-s r"    . consult-ripgrep)
              ;;("M-s l"    . consult-line)
              ;;("M-s m"    . consult-multi-occur)
              ;;("M-s k"    . consult-keep-lines)
              ;;("M-s u"    . consult-focus-lines)
              ;; Isearch integration
              ;;("M-s e"    . consult-isearch)
              )
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package wgrep)

(use-package company
  :config
  (global-company-mode 1))

(use-package outshine)

(use-package exec-path-from-shell
  :if (string-equal system-type "darwin")
  :config
  (exec-path-from-shell-initialize))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        (list (from-userdir "snip")))
  (yas-global-mode 1))

(use-package vterm
  :if (string-equal system-type "gnu/linux"))

(use-package mini-modeline
  :config
  (setq display-time-format "%a %d %b %H:%M"
        display-time-default-load-average nil
        mini-modeline-r-format
        '("%e"
          mode-line-front-space
          mode-line-mule-info
          mode-line-client
          mode-line-modified
          mode-line-remote
          mode-line-frame-identification
          mode-line-buffer-identification " "
          mode-name " "
          mode-line-position " "
          mode-line-misc-info))
  (display-time-mode 1)
  (mini-modeline-mode 1))

(use-package ace-window
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit    ace-jump-face-foreground
                      :height     4.5
                      :foreground "red"))))))

(use-package geiser-guile)

(use-package geiser)

(use-package guix)

;;; Winmark
(defun winmark-title (current-state)
  "Format open buffer names in CURRENT-STATE for bookmarks."
  (format "Winmark {%s}" (window-state-buffers current-state)))

(defun winmark-make-record ()
  "Record the current window configuration as a bookmark."
  (let* ((current-state (window-state-get nil t))
         (title         (winmark-title current-state)))
    `(,title
      (location . ,current-state)
      (handler  . winmark-handler))))

(defun winmark-handler (record)
  "Jump to a window bookmark RECORD."
  (-> record
      (bookmark-prop-get 'location)
      window-state-put))

(defun winmark-set ()
  "Create a window configuration bookmark."
  (interactive)
  (let ((bookmark-make-record-function #'winmark-make-record))
    (call-interactively 'bookmark-set)))

(bind-key "v" 'winmark-set 'leader-frames-map)

;;; Desktop
(setq desktop-restore-frames nil)
;;(desktop-save-mode 1)

;;; EXWM
(use-package exwm
  :if (equal (getenv "EXWM_ENABLE") "true")
  :config
  (require 'exwm-randr)
  (exwm-randr-enable)

  (setq exwm-layout-show-all-buffers      t
        exwm-workspace-show-all-buffers   t
        exwm-workspace-number             4
        exwm-randr-workspace-output-plist '(1 "LVDS-1"
                                              2 "HDMI-2")
        exwm-input-global-keys
        `((,(kbd "s-SPC") . leader-command-map)
          (,(kbd "<XF86AudioRaiseVolume>") . exwm-init/volume-up)
          (,(kbd "<XF86AudioLowerVolume>") . exwm-init/volume-down)
          (,(kbd "<XF86AudioMute>") . exwm-init/volume-mute)
          (,(kbd "s-r") . exwm-reset)
          (,(kbd "s-w") . exwm-workspace-switch)
          (,(kbd "s-&") . run-command)))

  (add-hook 'exwm-screen-change-hook 'exwm-init/screen-change)
  (add-hook 'exwm-update-class-hook 'exwm-init/buffer-name-update)

  (defun exwm-init/xrandr-cmd ()
    "Format display arguments for xrandr."
    (let ((outputs (seq-filter 'stringp exwm-randr-workspace-output-plist)))
      (concat "xrandr --output "
              (string-join outputs " --right-of ")
              " --auto")))

  (defcmd exwm-init/screen-change
    (exwm-init/xrandr-cmd))

  (defcmd exwm-init/volume-up
    "pactl set-sink-volume '@DEFAULT_SINK@' +5%")

  (defcmd exwm-init/volume-down
    "pactl set-sink-volume '@DEFAULT_SINK@' -5%")

  (defcmd exwm-init/volume-mute
    "pactl set-sink-mute '@DEFAULT_SINK@' toggle")

  (defun exwm-init/buffer-name-update ()
    "Set buffer name to window class."
    (exwm-workspace-rename-buffer exwm-class-name))

  ;; Enable EXWM
  (exwm-enable))

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
(use-package w3m)

(use-package web-search
  :disabled t
  :init (setq web-search-default-provider "DuckDuckGo")
  :bind ("C-c w" . web-search))

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
(set-face-background 'show-paren-match "#432121")
(setq show-paren-style 'expression)

;;;; IDE things
(use-package project-el
  :disabled t
  :straight (project-el :type git
                        :host github
                        :repo "jorgenschaefer/project-el"
                        :files ("project.el")))

(use-package eglot
  :after (project-el))

(use-package flycheck
  :config
  (setq flycheck-display-errors-function nil)
  (global-flycheck-mode 1))

(use-package expand-region
  :bind (:map leader-command-map
              ("." . 'er/expand-region)))

(use-package projectile)

(use-package magit
  :config (setq project-switch-commands nil))

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
  :disabled t
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

(use-package svelte-mode
  :disabled t)

(use-package tide
  :after (rjsx-mode flycheck)
  :hook ((rjsx-mode . tide-setup)
         (rjsx-mode . tide-hl-identifier-mode)))

(use-package php-mode)
(use-package web-mode)

;;;; Snake
(setq python-indent-offset 2)

;;;; Gophers
(use-package go-mode)

;;;; Rust
(use-package rust-mode)

;;;; Zig
(use-package zig-mode
  :disabled t
  :after (eglot)
  :config
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls")))
  (add-hook 'zig-mode-hook 'eglot-ensure))
