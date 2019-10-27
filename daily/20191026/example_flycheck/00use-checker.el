(require 'flycheck)
(flycheck-get-checker-for-buffer);; => emacs-lisp
(flycheck-checker-get 'python-mypy 'command);; => ("mypy" "--show-column-numbers" (config-file "--config-file" flycheck-python-mypy-ini) (option "--cache-dir" flycheck-python-mypy-cache-dir) source-original)

(flycheck-checker-shell-command 'python-mypy);; => "mypy --show-column-numbers VENV/individual-sandbox/daily/20191026/example_flycheck/00use-checker.el"

(flycheck-checker-executable 'python-flake8);; => "python"
(flycheck-checker-executable-variable 'python-flake8);; => flycheck-python-flake8-executable
(flycheck-checker-default-executable 'python-flake8);; => "python"
(flycheck-find-checker-executable 'python-flake8);; => "/usr/bin/python"

(funcall flycheck-executable-find "flake8");; => "VENV/bin/flake8"
(funcall flycheck-executable-find "mypy");; => "VENV/bin/mypy"

;;
(gethash "flake8" my:flycheck-executable-find-function-table) ;; => (lambda nil (cond ((boundp (quote my:flake8-path)) my:flake8-path) (t (setq my:flake8-path (or (pickup:pickup-file "bin/flake8") "flake8")) my:flake8-path)))

