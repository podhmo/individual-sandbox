# emacs major-mode, minor-mode

## major-mode

```lisp
(defvar foo-font-lock-keywords `((,(regexp-opt '("foo" "Foo" "bar" "Bar" "Boo" "boo")) . font-lock-keyword-face)))

(define-derived-mode foo-mode fundamental-mode "foo" "sample of major mode"
  (set (make-local-variable 'font-lock-defaults) '((foo-font-lock-keywords)))
  (set (make-local-variable 'comment-start) "#")
  (font-lock-mode)
)

; Bar Boo foo bar boo baz waaa
```

参考

- [emacswiki Mode Tutorial](https://www.emacswiki.org/emacs/ModeTutorial)
- [How to Write a Emacs Major Mode for Syntax Coloring](http://ergoemacs.org/emacs/elisp_syntax_coloring.html)


## minor-mode

```lisp
(defvar  htodo-font-lock-keywords `((,(regexp-opt (list "todo" "ToDo" "Todo" "TODO")) . font-lock-warning-face)))

(defun htodo-turn-on ()
  (font-lock-add-keywords nil `(,@htodo-font-lock-keywords) t)
  )

(defun htodo-turn-off ()
  (font-lock-remove-keywords nil `(,@htodo-font-lock-keywords))
  )

(define-minor-mode htodo-mode
  "htodo-mode (minor version)" :lighter " htodo"
  (progn
    (if htodo-mode (htodo-turn-on) (htodo-turn-off))
    (font-lock-mode 1)
    )
  )
```

参考

- [rainbow-mode/rainbow-mode.el at master · emacsmirror/rainbow-mode](
https://github.com/emacsmirror/rainbow-mode/blob/master/rainbow-mode.el)
