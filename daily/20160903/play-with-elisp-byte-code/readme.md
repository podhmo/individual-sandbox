## はじめに

elispのcodeを一部書き換えて保存したかった。

## やりたいこと

以下の事がしたい。(not macro)

1. symbolから関数の定義を取得する
2. 取得した関数のコードを書き換える
3. 書き換えた結果を利用する

```lisp
;; symbolから関数の定義を取得する
(symbol-function 'f) 
```

問題があってbyte codeになっているものは直接S式になっていない。

## byte code

```
(defun f () '(1 2 3))
(f); => (1 2 3)

;; byte codeにする
(fset 'bf (byte-compile 'f))
(bf); => (1 2 3)

(byte-code-function-p bf); => t
```

内部表現的にはvectorなのだけれど。vectorとしては扱えないのが面倒。

```
;; arefは使える
(aref bf 0); => nil

;; asetはダメ
(aset bf 0 nil) ;; wrong type argument: arrayp

(arrayp bf); => nil
(vectorp bf); => nil
```

実際のところ内部表現的にはすごく素朴なもので以下の様になっている。

```lisp
bf; => #[nil "\300\207" [(1 2 3)] 1]
```

これは直接リテラルとしても使える

```lisp
(fset 'bf2 #[nil "\300\207" [(1 2 3)] 1])
(bf2); => (1 2 3)
```

そして byte-code-functionからvectorへはvconcatで、vectorからbyte-code-functionへはmake-byte-codeを使うことで無理矢理変換できる。

```lisp
(let* ((v (vconcat (symbol-function 'bf2)))
      (code (aref v 2)))
  (aset code 0 '(1 2 3 4 5))
  (fset 'bf3 (make-byte-code (aref v 0) (aref v 1) code (aref v 3)))
  )
(bf3); => (1 2 3 4 5)
```

## ところで直接byte-compileした時に生成される.elcの中身はどのようなものかというと

例えば、以下のようなファイルをbyte-compileする。

```lisp
;; f.el
(defun f () '(a b c))
```

雑にbyte-compile-fileを使う。

```lisp
(byte-compile-file "./f") ;; f.elc作成
```

生成された.elcのファイルは、以下のようになっている。
fsetではなくdefaliasが使われている。なのでfsetよりはdefaliasを使ったほうが良いかもしれない。
(違いはdefalias-fset-functionのpropertyを見るか見ないかなので普通に使う分にはどちらでも構わない感じ)

```lisp
;ELC   
;;; Compiled
;;; in Emacs version 24.5.1
;;; with all optimizations.

;;; This file uses dynamic docstrings, first added in Emacs 19.29.

;;; This file does not contain utf-8 non-ASCII characters,
;;; and so can be loaded in Emacs versions earlier than 23.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defalias 'f #[nil "\300\207" [(a b c)] 1])
```

## defaliasを書く際にsymbol-functionを使う場合と使わない場合

symbol-functionを使う場合と使わない場合で違いがある。

```
(defalias 'g 'f)
(defalias 'g (symbol-function 'f))
```

例えばfletを使って違う処理を行いたい時などに分かるかもしれない。

例えば以下の様な状況の時

- 関数f()の挙動をちょっと変えたい。
- 自分勝手に定義したf()の代替のmy:f()を作成
- ただしmy:f()の中で元のf()を使いたい。

こういう場合にはsymbol-functionを使わないとダメ。

```lisp
(defun my:f (x)
    ;; do something
    (f x)) ;;　無限再帰

(defalias 'f-original 'f)
; (defalias 'f-original (symbol-function 'f))
(defalias 'f 'my:f)
```

以下の様な感じに

```lisp
(defun f (x)
  (cons "f" x))

(unless (fboundp 'f-original)
  (defalias 'f-original (symbol-function 'f))
  )

(defun my:f (x)
  (f-original (cons "my" x))
  )

(f '(10)); => ("f" 10)
(defalias 'f 'my:f)
(f '(10)); => ("f" "my" 10)
```
