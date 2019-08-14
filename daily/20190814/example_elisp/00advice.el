(defun add (x y)
  (+ x y))

(defun pp-filter (&rest args)
  (print (format "** %s" args))
  )

(advice-add 'add :before #'pp-filter)
;; (advice-remove 'add #'pp-filter)
(add 10 2)
