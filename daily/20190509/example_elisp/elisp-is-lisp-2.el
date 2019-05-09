;; emacs lispはlisp-2 (schemeはlisp-1)みたいな話

;; errorのときにはそのままエラーを値として返すマクロ
;; (ignore-errorsに近い)
;; 本当はerrの部分をgensymしたほうが良い(危険)。
(defmacro or-errors (&rest body)
  (declare (debug t) (indent 0))
  `(condition-case err (progn ,@body) (error err)))

;; 最初は何も束縛されていない

(or-errors (symbol-value 'x)) ;; => (void-variable x)
(symbol-function 'x) ;; => nil

;; setqは値の方の空間に束縛する
;; (symbol-function, symbol-valueはsymbolをkeyにそれぞれの空間を探索する)
;; 10が束縛されるのは値の方にだけ

(setq x 10)
(symbol-value 'x) ;; => 10
(symbol-function 'x) ;; => nil

;; defunなどはmacroで関数のほうの空間に束縛するのはfset
;; 今度はsymbol-functionで取り出せる

(fset 'x (function (lambda () 100)))
(symbol-value 'x) ;; => 10
(symbol-function 'x) ;; => (lambda nil 100)

;; 値として取り出せば10だし、関数として実行されれば100を返す関数として扱われる。

(list x) ;; => (10)
(x) ;; => 100

;; funcallは値の方に格納されたものをみる。
;; symbolが渡されたら関数の空間の方を見る

(or-errors (funcall x)) ;; => (invalid-function 10)
(or-errors (funcall 'x)) ;; => 100
(or-errors (funcall #'x)) ;; => 100

;; 'も#'も単なる省略形
;; (function ...)は単にbyte-compileのときのためのちょっとした便利表現というだけ

(macroexpand '(list y 'y #'y));; => (list y (quote y) (function y))

;; 値のほうの空間にlambda式を格納した場合

(setq f (lambda (y) (list y y)))
(symbol-value 'f) ;; => (lambda (y) (list y y))
(symbol-function 'f) ;; => nil

(or-errors (f 10)) ;; => (void-function f)
(funcall f 10) ;; => (10 10)
(or-errors (funcall 'f 10)) ;; => (void-function f)
(or-errors (funcall #'f 10)) ;; => (void-function f)
