(require 'cl)

(defun f ()
  (backtrace)
)

(cl-flet ((backtrace () (message "hai")))
  (f))

(flet ((backtrace () (message "hai")))
  (f));; => "hai"

(cl-letf (((symbol-function 'backtrace) (lambda () (message "hai"))))
  (f));; => "hai"

;; (macroexpand-all
;;  '(cl-flet ((backtrace () (message "hai")))
;;    (f)
;;    ))

;; (let* ((--cl-backtrace-- (function (lambda nil (message "hai")))))
;;   (progn (f)))

;; (let* ((vnew (function (lambda nil (message "hai"))))
;;        (old (symbol-function (quote backtrace))))
;;   (unwind-protect
;;       (progn (fset (quote backtrace) vnew) (f))
;;     (fset (quote backtrace) old)))
