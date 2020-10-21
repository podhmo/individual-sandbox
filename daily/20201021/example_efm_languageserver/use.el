(use-package eglot
  :ensure t
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(markdown-mode . ("efm-langserver" "-logfile"  "/tmp/efm.log" "-loglevel" "10")))
    (add-hook 'markdown-mode-hook 'eglot-ensure)
    ))
