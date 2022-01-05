;;; init.el --- my init file -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; loads my packages
;;;
;;; Code:
;; * Loading packages
(setq init-packages '(guix setup exec-path-from-shell which-key
                           hydra project projectile magit vterm
                           w3m vertico marginalia orderless
                           consult embark company
                           frames-only-mode undo-tree wgrep
                           yasnippet outshine yaml-mode
                           markdown-mode rainbow-delimiters
                           smartparens flycheck eglot
                           expand-region nix-mode geiser-guile
                           geiser racket-mode cider
                           typescript-mode php-mode web-mode
                           restclient go-mode lua-mode))

(require 'seq)

;; ** Guix
(when (require 'guix-repl nil t)
  (defvar guix-init-profile
    (expand-file-name "init-package-profile" user-emacs-directory))

  (defun guix-emacs-profile-packages (profile)
    "Get the emacs packages installed in PROFILE."
    (let* ((script `(begin
                     (use-modules (guix profiles))
                     (manifest->code (profile-manifest ,profile))))
           ;; god why
           (manifest (cdr (cadadr (guix-eval-read (prin1-to-string script)))))
           (pkgs (mapcar (lambda (s)
                           (intern (seq-subseq s 6)))
                         (seq-filter (apply-partially #'string-match "^emacs-.*")
                                     manifest))))
      pkgs))

  (defun guix-emacs-add-profile (profile)
    "Add PROFILE to the load path."
    (interactive (list guix-init-profile))
    (let ((site-lisp (expand-file-name "share/emacs/site-lisp" profile)))
      (add-to-list 'load-path site-lisp)
      (load (expand-file-name "subdirs" site-lisp))))

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
                       (specifications->manifest (list "emacs" ,@specs)))))))
      (guix-eval-read (prin1-to-string script))))

  (defun guix-emacs-update-packages (pkgs)
    "Sync the init profile and autoload all PKGS."
    (interactive (list init-packages))
    (guix-emacs-build-profile guix-init-profile pkgs)
    (guix-emacs-add-profile guix-init-profile)
    (guix-emacs-autoload-packages))

  (when (not (equal init-packages (guix-emacs-profile-packages guix-init-profile)))
    (guix-emacs-update-packages init-packages)))

;; ** Straight
(defun straight-update-packages (pkgs)
  "Sync straight with the PKGS list and autoload all packages."
  (seq-do #'straight-use-package pkgs))

;; * Environment
(require 'setup)

(setup init/bootstrap
  (:require xdg subr-x)

  (when (string-equal system-type "darwin")
    (:require exec-path-from-shell)
    (exec-path-from-shell-initialize))

  (put 'thread-first 'lisp-indent-function nil)
  (put 'thread-last 'lisp-indent-function nil)

  (defalias '-> 'thread-first)
  (defalias '->> 'thread-last)
  (defalias 'do 'progn))

(setup setup/extras
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
    :shorthand #'cadr))

;; * Helping hands
(defmacro comment (&rest _)
  "Ignore anything inside here."
  nil)

(defmacro doto (init &rest forms)
  "Evaluate INIT and thread it into FORMS, return the evaluated INIT."
  (declare (indent 1))
  (let ((init-var (make-symbol "init-var")))
    `(let ((,init-var ,init))
       ,@(mapcar (lambda (form)
                   `(-> ,init-var ,form))
                 forms)
       ,init-var)))

(defun read-first (fname)
  "Read first object from file FNAME."
  (with-temp-buffer
    (insert-file-contents fname)
    (read (current-buffer))))

(defun rotate-left (vs)
  "Rotate VS by one element."
  (append (cdr vs)
          (-> (car vs)
              list)))

(defun unbind-all (sym)
  "Remove all bindings from SYM."
  (fmakunbound sym)
  (makunbound sym))

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
  (find-file user-init-file))

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
  (doto user-init-file
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

;; * The Journal
(comment

 (->> (file-attributes guix-init-profile)
      (nth 4)
      current-time-string)

 ...)

;; * General customizations
(setup init/custom
  ;; y is y
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; narrow?
  (put 'narrow-to-region 'disabled nil)

  ;; colors!
  (load-theme 'wombat t)

  ;; font size
  (set-face-attribute 'default nil :height 120)

  ;; flat modeline
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)

  (:option
   ;; truncate minibuffer display
   message-truncate-lines t

   ;; backups and temp files
   backup-directory-alist `((".*" . ,(expand-file-name "emacs/backup" (xdg-cache-home))))
   auto-save-list-file-prefix (expand-file-name "emacs/auto-save-list/.saves-" (xdg-cache-home))
   auto-save-default nil
   create-lockfiles nil
   backup-by-copying t     ; Don't delink hardlinks
   version-control t       ; Use version numbers on backups
   delete-old-versions t   ; Automatically delete excess backups
   kept-new-versions 20    ; how many of the newest versions to keep
   kept-old-versions 5     ; and how many of the old

   ;; trailing whitespace
   (prepend before-save-hook) #'delete-trailing-whitespace

   ;; no bells pls
   ring-bell-function #'ignore

   ;; shrink fringes to 1 pixel
   fringe-mode 1

   ;; turn off stupid bars (in order of greatest annoyance)
   tool-bar-mode nil
   menu-bar-mode nil
   scroll-bar-mode nil

   ;; line flow
   fill-column 80

   ;; self-care
   inhibit-startup-screen t
   inhibit-startup-echo-area-message "This aint your dad's spacemacs"
   initial-scratch-message ";; 頑張って!\n\n"
   initial-buffer-choice (lambda () (dired (getenv "HOME")))))

;; * Keys
(setup (:use-global-mode which-key-mode))

(setup (:minor-mode global-keys-mode
	       "Mode for global keybinds without messing with the global keymap."
	       :global t)

  (:keymap leader-command-map leader-buffers-map)

  (:bind
   ;; Ibuffer
   [remap list-buffers] ibuffer
   ;; Font size
   "C-=" text-scale-increase
   "C--" text-scale-decrease
   "C-+" (lambda nil (interactive) (text-scale-set 0))
   ;; Config
   "<f9>" config-reinit
   "<f12>" edit-user-config
   ;; Leader
   "M-SPC" leader-command-map)

  (:with-map leader-command-map
    (:bind "<SPC>" execute-extended-command
           "m" bookmark-map
           "h" help-map
           "b" leader-buffers-map))

  (:with-map leader-buffers-map
    (:bind "f" find-file
           "g" projectile-find-file
           "b" switch-to-buffer
           "r" revert-buffer
           "k" kill-this-buffer-now
           "s" save-buffer
           "w" write-file))

  (:with-map bookmark-map
    (:bind "B" bookmark-jump-nosave))

  (:hydra hydra-edit ("<C-backspace>" :foreign-keys run)
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

  (:hydra hydra-sp-edit ("<C-S-backspace>" :foreign-keys run)
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

  (:use-global-mode global-keys-mode))

;; * Utilities
;; ** Workspaces
(setup tramp
  (:option tramp-persistency-file-name (expand-file-name "emacs/tramp" (xdg-cache-home))))

(setup dired
  (defun mime-open-file (path)
    "Open file at PATH with an appropriate application."
    (make-process :name "open-file"
                  :command (list "xdg-open" path)))

  (defun dired-open-at-point ()
    "Opens the file under the point with `mime-open-file`."
    (interactive)
    (-> (dired-filename-at-point)
        mime-open-file))

  (:option dired-listing-switches "-alh")
  (:bind "C-c C-o" dired-open-at-point))

(setup package
  (:option project-switch-commands nil))

;; ** Shell
(setup eshell
  (defun eshell/clear ()
    "Actually clear eshell."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;; ** Innernet
(setup web-search
  (:only-if nil)
  (:option web-search-default-provider "DuckDuckGo")
  (:bind "C-c w" web-search))

;; * Completion
(setup (:use-global-mode vertico marginalia)
  (:with-feature minibuffer
    (:with-map minibuffer-local-map
      (:bind "C-t" marginalia-cycle)))

  (:option completion-styles '(orderless)))

(setup consult
  (:with-feature global-keys-mode
    (:bind "C-s" consult-line)

    (:with-map leader-command-map
      (:bind "n" consult-line)))

  (:with-feature outline
    (:with-map outline-minor-mode-map
      (:bind "C-c s" (lambda ()
                       (interactive)
                       (consult-outline)
                       (outline-hide-other))))))

(setup embark
  (:with-feature minibuffer
    (:with-map minibuffer-local-map
      (:bind "C-." embark-act))))

(setup company
  (:use-global-mode global-company-mode))

;; * Windows and frames
(setup (:use-global-mode frames-only-mode))

(setup desktop
  (:only-if nil)
  (:use-global-mode desktop-save-mode)
  (:option desktop-restore-frames nil))

;; * Editing
(setup undo-tree
  (:use-global-mode global-undo-tree-mode)
  (:option undo-tree-history-directory-alist `((".*" . ,(expand-file-name "emacs/undo" (xdg-cache-home))))))

(setup yasnippet
  (:use-global-mode yas-global-mode)
  (:option yas-snippet-dirs (list (expand-file-name "snip" user-emacs-directory))))

(setup org
  (:option org-babel-confirm-evaluate nil
           org-image-actual-width '(400)))

(setup yaml-mode
  (:file-match "\\.yml\\'"))

(setup markdown-mode
  (:file-match "\\.md\\'")
  (:option markdown-hide-markup nil
           markdown-hide-urls t))

;; * Programming
;; ** General
(setup prog-mode
  (:option indent-tabs-mode nil
           tab-width 2
           standard-indent 2
           sh-basic-offset 2
           python-indent-offset 2)

  (:hook display-line-numbers-mode
         rainbow-delimiters-mode
         smartparens-mode
         show-paren-mode
         outshine-mode)

  (:bind "C-M-k" sp-forward-slurp-sexp
         "C-s-k" sp-forward-barf-sexp
         "C-M-j" sp-backward-slurp-sexp
         "C-s-j" sp-backward-barf-sexp))

(setup paren
  (:when-loaded
    (:option show-paren-style 'expression)
    (set-face-background 'show-paren-match "#432121")))

(setup flycheck
  (:use-global-mode global-flycheck-mode)
  (:require flycheck-clj-kondo)
  (:option flycheck-display-errors-function nil))

(setup eglot
  (:when-loaded
    (:option (prepend eglot-server-programs) '(typescript-mode . (eglot-deno "deno" "lsp")))

    (defclass eglot-deno (eglot-lsp-server) ()
      :documentation "A custom class for deno lsp.")

    (cl-defmethod eglot-initialization-options ((server eglot-deno))
      "Passes through required deno initialization options"
      '(:enable t :lint t))))

(setup expand-region
  (:with-feature global-keys-mode
    (:with-map leader-command-map
      (:bind "." er/expand-region))))

;; ** Lisp
(setup racket-mode
  (:bind "C-x C-e" racket-eval-last-sexp))

(setup cider
  (:require ob-clojure)

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

  (:hook cider-prestart)

  (:bind "C-, e" cider-eval-commands-map)
  (:with-map cider-eval-commands-map
    (:bind "k" cider-eval-defun-output-to-kill-ring))

  (:option
   cider-print-fn 'fipp
   clojure-indent-style 'align-arguments
   org-babel-clojure-backend 'cider
   clojure-toplevel-inside-comment-form t
   cider-repl-pop-to-buffer-on-connect nil

   ;; Format pretty-printed comments to appear as evaluation output
   cider-comment-prefix "\n  #_=> "
   cider-comment-continued-prefix "       "
   cider-comment-postfix "\n"

   cider-clojure-cli-aliases "dev"
   cider-shadow-cljs-command "clojure -M:shadow-cljs"))

;; ** Javascript
(setup js
  (:option js-indent-level 2))

(setup typescript-mode
  (:option typescript-indent-level 2))

;; ** Lua
(setup lua-mode
  (:option lua-indent-level 2))

;; * End
(provide 'init)
;;; init.el ends here
