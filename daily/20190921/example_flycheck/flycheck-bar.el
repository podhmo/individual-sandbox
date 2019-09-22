;;; flycheck-bar.el --- flycheck for schemalint -*- lexical-binding: t -*-

;;; Code:

(require 'generic-x)
(require 'flycheck) ; For `flycheck-define-checker'
(define-generic-mode
    'bar-mode
  '("#")
  '()
  '()
  '("\\.bar$")                      ;; files for which to activate this mode
  nil                              ;; other functions to call
  "A mode for bar files"            ;; doc string for this mode
  )

(defun bar:parse (output checker buffer)
  (when (not (string-empty-p output))
    (let (errors)
      (dolist (data (flycheck-parse-json output))
        (let-alist data
          (let ((status (intern (downcase (or .status "INFO")))))
            (push
             (flycheck-error-new-at
              .line 1
              status
              .message
              :checker checker
              :buffer buffer
              ;; :group?, :id?
              )
             errors))
          ))
      (nreverse errors))))

(flycheck-define-checker bar
  "A Bar linter using schemalint

See URL `https://github.com/podhmo/schemalint'.
"
  :command ("python" "/home/nao/venvs/my/individual-sandbox/daily/20190921/example_flycheck/lint.py" "--json")
  :standard-input t
  :error-parser bar:parse
  :modes bar-mode)

(add-to-list 'flycheck-checkers 'bar)
(defun my:bar-mode-setup ()
  (flycheck-mode t)
  )
(add-hook 'bar-mode-hook 'my:bar-mode-setup)
(provide 'flycheck-bar)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-bar.el ends here
