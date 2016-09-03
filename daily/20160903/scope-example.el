(require 'cl)

(setq y "global-y")

(defun f (x) (g x))
(defun g (x) (list x "global" y))

(f 10); => (10 "global" "global-y")

(flet ((g (x) (list x "local" y)))
  (let ((y "local-y"))
    (f 10))); => ("global" 10)

(cl-flet ((g (x) (list x "local" y)))
  (let ((y "local-y"))
    (f 10))); => (10 "global" "local-y")
