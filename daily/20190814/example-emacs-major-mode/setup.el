;; 色々調べるのに便利そうな関数を読み込むだけ
(load (concat (current-directory) "util.el"))

(use-package foo-mode
  :load-path default-directory
  :mode (("\\.foo$" . foo-mode))
  :config
  (add-to-list 'exec-path (string-remove-suffix "/" (current-directory)))
  (assert (executable-find "foo-lint"))

  (defun my:foo-mode-setup ()
    (interactive)
    (flycheck-mode-on-safe)
    (remove-hook 'flymake-diagnostic-functions 'foo-flymake)
    ;; (add-hook 'flymake-diagnostic-functions 'foo-flymake nil t)
    ;; (flymake-mode 1)
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

  (defun my:flycheck-trace (&rest args)
    (with-current-buffer (get-buffer-create "*FLYCHECK TRACE*")
      (goto-char (point-max))
      (let ((time (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))
        (insert (format "%s:	start flycheck	with	%s\n" time args))
        )))

  (advice-add 'flycheck-buffer-automatically :before #'my:flycheck-trace)
  ;; (advice-remove 'flycheck-buffer-automatically #'my:flycheck-trace)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-idle-change-delay 0.5)
  (setq flycheck-idle-buffer-switch-delay 0.5)
  (setq flycheck-hooks-alist
        (delq (assoc 'after-change-functions flycheck-hooks-alist) flycheck-hooks-alist))
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

  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  )
