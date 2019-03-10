## emacs swiper

```
(require 'swiper)
```

## emacs undo,redo

- undo-propose
- undo-tree

undo-treeも使ってみるか

```lisp
(require 'undo-tree)
(global-undo-tree-mode)
```

どうやって使うものなんだろう？

- `M-x undo-tree-undo`
- `M-x undo-tree-redo`
- `M-x undo-tree-visualize`

## go eglot bingo

install

```console
$ go get -u -v github.com/saibing/bingo
```

```
(require 'eglot)

;; bingo
;; (add-to-list 'eglot-server-programs '(go-mode "bingo" "-format-style" "goimports" "-trace"))
(add-to-list 'eglot-server-programs '(go-mode "bingo" "-format-style" "goimports"))
```

- eglotのlintを切る方法
- elotで変わる機能

```
;; periodically to provide enhanced code analysis via
;; `xref-find-definitions', `flymake-mode', `eldoc-mode',
;; `completion-at-point', among others.
```

eglot--managed-mode付近を見ると良い

```lisp
(eglot--all-major-modes)
```
