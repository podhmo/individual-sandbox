;; 色々調べるのに便利そうな関数を読み込むだけ
(load (concat (current-directory) "util.el"))

(use-package foo-mode
  :load-path default-directory
  :mode (("\\.foo$" . foo-mode))
  :config
  (add-to-list 'exec-path (string-remove-suffix "/" (current-directory)))
  (assert (executable-find "foo-lint"))

  (defun my:foo-mode-setup ()
    ;; (flycheck-mode-on-safe)
    (add-hook 'flymake-diagnostic-functions 'foo-flymake nil t)
    ;; (remove-hook 'flymake-diagnostic-functions 'foo-flymake)
    (flymake-mode 1)
    )
  (add-hook 'foo-mode-hook 'my:foo-mode-setup)
  )

;; flycheck
(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :config
  (flycheck-define-checker foo-lint
                           "lint for .foo"
                           :command ("foo-lint" source-inplace)
                           :error-patterns
                           ((error line-start (file-name) ":" line ":" (message) line-end))
                           :modes foo-mode)

  (add-to-list 'flycheck-checkers 'foo-lint)
  ;; M-x flycheck-verify-setup, flycheck-verify-checker
  )


;; flymake
(use-package flymake
  :ensure t
  :commands (flymake-mode)
  :config
  (load (concat (current-directory) "foo-flymake"))
  ;; :emergency :error :warning :debug
  (setq warning-minimum-log-level :warning)
  (setq flymake-start-on-newline nil)
  (setq flymake-no-changes-timeout nil)
  )
