;; byte-code をちょっと弄りたい

(defun f () '(a b c))
(f); => (a b c)

(fset 'bf (byte-compile 'f))
(bf); => (a b c)

bf; => #[nil "\300\207" [(a b c)] 1]
(type-of bf); => compiled-function
(aref bf 1); => "\300\207"
(arrayp bf); => nil
(byte-code-function-p bf); => t

(aref bf 3); => 1
(aset bf 1 1) ;; wrong type argiment: arrayp 

;; see: http://www.geocities.co.jp/SiliconValley-Bay/9285/ELISP-JA/elisp_205.html
;; see: http://nullprogram.com/blog/2014/01/04/

bf; => #[nil "\300\207" [(a b c)] 1]
(make-byte-code nil "" [] 0); => #[nil "" [] 0]
(make-byte-code (aref bf 0) (aref bf 1) (aref bf 2) (aref bf 3)); => #[nil "\300\207" [(a b c)] 1]

;; (a b c) -> (x y z)
(let ((v (vconcat bf))
      (code (aref bf 2)))
  (aset code 0 '(x y z))
  (defalias 'bf2 (make-byte-code (aref v 0) (aref v 1) code (aref v 3)))
  )
(bf2); => (x y z)
