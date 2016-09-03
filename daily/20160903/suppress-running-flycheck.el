;; これ頑張ったけれど。 `flycheck-check-syntax-automatically' を調整すれば良いだけだった。


;; TODO: running checker two times, every changing, for auto-save and content edit
;; string -> process
(defvar my:debounced-process-map (make-hash-table :test 'equal :weakness 'value))
(defvar my:debounced-process-threashold 0.5)

(defun my:debounced-start-process (name buffer program &rest program-args)
  (let ((previous-process (gethash name my:debounced-process-map nil))
        (threshold my:debounced-process-threashold))
    (cond ((null previous-process)
           (apply 'my:debounced-start-process--on-success name buffer program-args program-args))
          (t
           (let* ((timestamp (process-get previous-process 'debounce-timestamp))
                 (n (- (float-time (current-time)) timestamp)))
             (cond ((> n threshold)
                    (apply 'my:debounced-start-process--on-success name buffer program-args program-args))
                   (t
                    ;; (message "my:flycheck: %s is debounced (< %s %s)" name n threshold)
                    previous-process)))))))

(defun my:debounced-start-process--on-success (name buffer program &rest program-args)
  (let1 timestamp (float-time (current-time))
    (rlet1 process (apply 'start-process name buffer program program-args)
      (puthash name process my:debounced-process-map)
      (process-put process 'debounce-timestamp timestamp))))


;; code transformer
(defun my:sexp-map (f sexp)
  (cond ((arrayp sexp)
         (let ((v (copy-sequence sexp)))
           (dotimes (i (length v))
             (let ((val (my:sexp-map f (aref v i))))
               (when (not (equal val (aref v i)))
                 (aset v i val))))
           v))
        ((listp sexp)
         (lexical-let ((f f))
           (mapcar (lambda (x) (sexp-map f x)) sexp)))
        ((atom sexp)
         (funcall f sexp))
        (t sexp)))

(defun my:byte-code-map (f bcode)
  (let* ((v (vconcat bcode))
         (code (aref bcode 2))
         (transformed-code (my:sexp-map f code)))
    ;; to byte code function
    (cond ((< 4 (length v))
           (make-byte-code (aref v 0) (aref v 1) transformed-code (aref v 3) (aref v 4)))
          (t
           (make-byte-code (aref v 0) (aref v 1) transformed-code (aref v 3))))
    ))

(defun my:code-map (f code)
  (cond ((byte-code-function-p code) (my:byte-code-map f code))
        (t (my:sexp-map f code))))


(unless (fboundp 'flycheck-start-command-checker-original)
  (defalias 'flycheck-start-command-checker-original (symbol-function 'flycheck-start-command-checker))
)
(defalias 'flycheck-start-command-checker
  (my:code-map (lambda (e) (if (eq 'start-process e) 'my:debounced-start-process e))
               (symbol-function 'flycheck-start-command-checker-original)))


;; start-process -> my:start-process
;; (cl-flet ((transform
;;            (code)
;;            (my:code-map (lambda (e) (if (eq 'start-process e) 'my:start-process e))
;;                         code))
;;           (detransform
;;            (code)
;;            (my:code-map (lambda (e) (if (eq 'my:start-process e) 'start-process e))
;;                         code))
;;           )
;;   (let ((x (symbol-function 'flycheck-start-command-checker)))
;;     (list
;;      :byte-compile? (byte-code-function-p x)
;;      :transform->modified? (not (equal (transform x) x))
;;      :transform->detransform->not-modified? (equal (detransform (transform x)) x))
;;     )); => (:byte-compile\? t :transform->modified\? t :transform->detransform->not-modified\? t)
