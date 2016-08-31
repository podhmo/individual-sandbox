(require 'cl)

(cl-defstruct zipper head tail)

(defun zipper-make-empty ()
  (make-zipper :head nil :tail nil))

(defun zipper-last-p (z)
  (null (zipper-tail z)))

(defun zipper-first-p (z)
  (null (zipper-head z)))

(defun zipper-forward (z)
  (cond ((zipper-last-p z) z)
        (t (make-zipper
            :head (cons (car (zipper-tail z)) (zipper-head z))
            :tail (cdr (zipper-tail z)))
           )))

(defun zipper-backward (z)
  (cond ((zipper-first-p z) z)
        (t (make-zipper
            :head (cdr (zipper-head z))
            :tail (cons (car (zipper-head z)) (zipper-tail z)))
           )))

(defun zipper-insert (z e)
  (make-zipper :head (cons e (zipper-head z)) :tail (zipper-tail z)))

(defun zipper-current (z)
  (let ((h (zipper-head z)))
    (cond ((null h) (car (zipper-tail z)))
          (t (car h)))))

;; TODO: test
;; * -> 1 -> 2 -> 3
;; '(() '(1 2 3))
;; 1 -> * ->  2 -> 3
;; '((1) '(2 3))
;; 1 -> 2 -> * -> 3
;; '((2 1) '(3))
;; 1 -> 2 -> 3 -> *
;; '((3 2 1) '())
;; 1 -> 2 -> 3 -> * -> 4
;; '((3 2 1) '(4))
;; (let* ((z0 (zipper-make-empty))
;;        (z1 (zipper-insert z0 1))
;;        (z2 (zipper-insert z1 2))
;;        (z3 (zipper-insert z2 3))
;;        (z4 (zipper-forward z3)))
;;   (let ((result (list z0 z1 z2 z3 z4)))
;;     (dolist (z result)
;;       (princ (zipper-current z)))))
