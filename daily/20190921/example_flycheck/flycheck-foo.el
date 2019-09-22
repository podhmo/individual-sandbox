;;; flycheck-foo.el --- flycheck for schemalint -*- lexical-binding: t -*-

;;; Code:

(require 'generic-x)
(require 'flycheck) ; For `flycheck-define-checker'
(define-generic-mode
    'foo-mode
  '("#")
  '()
  '()
  '("\\.foo$")                      ;; files for which to activate this mode
  nil                              ;; other functions to call
  "A mode for foo files"            ;; doc string for this mode
  )

(flycheck-define-checker foo
  "A Foo linter using schemalint

See URL `https://github.com/podhmo/schemalint'.
"
  :command ("python" "/home/nao/venvs/my/individual-sandbox/daily/20190921/example_flycheck/lint.py")
  :standard-input t
  :error-patterns
  ((info line-start
         "INFO" ":" line ":" (message)
         line-end
         )
   (warning line-start
            "WARNING" ":" line ":" (message)
            line-end
            )
   (error line-start
          "ERROR" ":" line ":" (message)
          line-end
          )
   )
  :modes foo-mode)

(add-to-list 'flycheck-checkers 'foo)
(defun my:foo-mode-setup ()
  (flycheck-mode t)
  )
(add-hook 'foo-mode-hook 'my:foo-mode-setup)
(provide 'flycheck-foo)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-foo.el ends here
