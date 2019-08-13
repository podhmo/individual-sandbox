(require 'generic-x)

(define-generic-mode
    'foo-mode                         ;; name of the mode to create
  '("#")                           ;; comments start with '!!'
  '("define")                     ;; some keywords
  '(("=|\\+" . 'font-lock-constant-face)     ;; '=' is an operator
    ("[0-9]+" . 'font-lock-variable-name-face)
    (";" . 'font-lock-builtin-face))     ;; ';' is a built-in
  '("\\.foo$")                      ;; files for which to activate this mode
  nil                              ;; other functions to call
  "A mode for foo files"            ;; doc string for this mode
  )

(provide 'foo-mode)
;;; foo-mode.el ends here
