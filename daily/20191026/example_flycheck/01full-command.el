(require 'flycheck)
(flycheck-checker-shell-command 'python-mypy);; => "mypy --show-column-numbers VENV/individual-sandbox/daily/20191026/example_flycheck/00use-checker.el"

(defun my:flycheck-wrapper-function--use-fullpath-command (command)
  ;; command :: (<cmd name> ,@args)
  (cons (funcall flycheck-executable-find (car command))
        (cdr command))
  )
(setq flycheck-command-wrapper-function #'my:flycheck-wrapper-function--use-fullpath-command)

(flycheck-checker-shell-command 'python-mypy);; => "VENV/bin/mypy --show-column-numbers VENV/individual-sandbox/daily/20191026/example_flycheck/01full-command.el"
