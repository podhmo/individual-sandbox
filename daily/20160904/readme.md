# daily

これをもう少しまともに書きたい。

- https://gist.github.com/podhmo/68d1367183274b861ed7cb043fd6ca2a

# js ocaml bucklescript

- [bloomberg/bucklescript: A backend for the OCaml compiler which emits JavaScript.](https://github.com/bloomberg/bucklescript)

js_of_ocamlとかとの関係がわからない。わりと綺麗なコード生成するんだろうか？

# emacs golang $GOPATH以下のsrcをanythingで選択

```lisp
(defvar my:anything-c-source-go-src-selection
  '((name . "Go src selection")
    (init
     . (lambda ()
         (let ((buf (anything-candidate-buffer " *go src*"))
               (cmd "find $GOPATH/src -type d -mindepth 3 -maxdepth 3"))
           (flet ((display-buffer (&rest args) nil))
             (shell-command cmd buf buf))
           )))
    (candidates-in-buffer)
    (type . file)))

(defun my:anything-go-src-selection ()
  (interactive)
  (let ((sources '(my:anything-c-source-go-src-selection)))
    (anything-other-buffer sources "*anything go packages*")))
```

# emacs emacsの起動を早くする

これが面白い。

- [Emacsのスタートアップを視覚的に理解する - Qiita](http://qiita.com/yuttie/items/0f38870817c11b2166bd)

```lisp
(require-and-fetch-if-not 'initchart :url "https://raw.githubusercontent.com/yuttie/initchart/master/initchart.el")
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)
;(initchart-visualize-init-sequence)
```

load,require以外の状況も把握しておきたい感じ

###  色々な関数を集める

```lisp
(progn
  (mapatoms (lambda (e) 
              (prin1 (list e (functionp e) (subrp e) (and (functionp e) (find-lisp-object-file-name e (symbol-function e)))))
              (terpri)))
  nil
  )
```
### 関数の引数の数を数えたい

```lisp
;; とりあえずdocstringを取る
(documentation 'anything)
```


### require,load以外の状況も見たい

```
(defmacro my:initchart-record-execution-all ()
  (let ((targets (list)))
    (mapatoms (lambda (sym)
                (when (and (functionp sym) (not (subrp (symbol-function sym))))
                  (push sym targets)
                  )
                ))
    (let ((body (mapcar (lambda (sym) `(initchart-record-execution-time-of ,sym)) targets)))
      `(progn
         ,@body
         )))
  )

;; 試す
(let (print-length print-depth (message-log-max 100000))
  (pp (macroexpand '(my:initchart-record-execution-all))))
```

結論としては失敗。全部にadviceを仕掛けるのは現実的ではない。

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
