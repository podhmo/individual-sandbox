(require 'pcase) ;; external package

(defun foo (x)
  (pcase x
    (`(,a) (list "one" a))
    (`(,a ,b) (list "two" a b))
    ((and args (pred listp)) (cons "many" args))
    ))

(foo (list 1)); => ("one" 1)
(foo (list 1 2)); => ("two" 1 2)
(foo (list 1 2 3)); => ("many" 1 2 3)
(foo 0); => nil
