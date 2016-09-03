(defun sexp-map (f sexp)
  (cond ((arrayp sexp)
         (let ((v (copy-sequence sexp)))
           (dotimes (i (length v))
             (let ((val (sexp-map f (aref v i))))
               (when (not (equal val (aref v i)))
                 (aset v i val))))
           v))
        ((listp sexp)
         (lexical-let ((f f))
           (mapcar (lambda (x) (sexp-map f x)) sexp)))
        ((atom sexp)
         (funcall f sexp))
        (t sexp)))

(defun byte-code-map (f bcode)
  (let* ((v (vconcat bcode))
         (code (aref bcode 2))
         (transformed-code (sexp-map f code)))
    ;; to byte code function
    (cond ((< 4 (length v))
           (make-byte-code (aref v 0) (aref v 1) transformed-code (aref v 3) (aref v 4)))
          (t
           (make-byte-code (aref v 0) (aref v 1) transformed-code (aref v 3))))
    ))

(defun code-map (f code)
  (cond ((byte-code-function-p code) (byte-code-map f code))
        (t (sexp-map f code))))



;; start-process -> my:start-process
(defun transform (code)
  (code-map (lambda (e) (if (eq 'start-process e) 'my:start-process e))
           code))

(defun detransform (code)
  (code-map (lambda (e) (if (eq 'my:start-process e) 'start-process e))
            code))


(defun f () '(x y z start-process))

(let ((x (symbol-function 'f)))
  (list
   :byte-compile? (byte-code-function-p x)
   :transform->modified? (not (equal (transform x) x))
   :transform->detransform->not-modified? (equal (detransform (transform x)) x))
  ); => (:byte-compile\? nil :transform->modified\? t :transform->detransform->not-modified\? t)

(let ((x (byte-compile (symbol-function 'f))))
  (list
   :byte-compile? (byte-code-function-p x)
   :transform->modified? (not (equal (transform x) x))
   :transform->detransform->not-modified? (equal (detransform (transform x)) x))
  ); => (:byte-compile\? t :transform->modified\? t :transform->detransform->not-modified\? t)

(let ((x (symbol-function 'flycheck-start-command-checker)))
  (list
   :byte-compile? (byte-code-function-p x)
   :transform->modified? (not (equal (transform x) x))
   :transform->detransform->not-modified? (equal (detransform (transform x)) x))
  ); => (:byte-compile\? t :transform->modified\? t :transform->detransform->not-modified\? t)
