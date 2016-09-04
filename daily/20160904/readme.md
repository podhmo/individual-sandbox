# emacs major-mode

```lisp
(setq foo-highlights `((,(regexp-opt '("foo" "Foo" "bar" "Bar" "Boo" "boo")) . font-lock-keyword-face)))

(define-derived-mode foo-mode fundamental-mode "foo" "sample of major mode"
  (set (make-local-variable 'font-lock-defaults) '((foo-highlights)))
  (set (make-local-variable 'comment-start) "#")
  (font-lock-mode)
)

; Bar Boo foo bar boo baz waaa
```

参考

- [emacswiki Mode Tutorial](https://www.emacswiki.org/emacs/ModeTutorial)
- [How to Write a Emacs Major Mode for Syntax Coloring](http://ergoemacs.org/emacs/elisp_syntax_coloring.html)



