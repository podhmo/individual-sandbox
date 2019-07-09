;; M-x package-install lsp-go lsp-ui lsp-mode
(require 'lsp-mode)
(require 'lsp-ui)

(remove-hook 'go-mode-hook 'my:go-mode-setup)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(defun my:individual-go-setup ()
  (company-mode-on)
  (bind-keys :map go-mode-map
             ("C-x C-s" . lsp-format-buffer))
  (lsp-ui-sideline-mode -1)
)
(add-hook 'go-mode-hook 'my:individual-go-setup)

(custom-set-variables `(lsp-clients-go-server-args ("-rpc.trace" "-logfile" "/tmp/gopls.log" "-debug" "localhost:6060")))
