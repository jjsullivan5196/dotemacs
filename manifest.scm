(specifications->manifest
 (cons* "emacs" "emacs-guix"
        (map (lambda (name)
               (string-append "emacs-" (symbol->string name)))
             (load "./packages"))))
