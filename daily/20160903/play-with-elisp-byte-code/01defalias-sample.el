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
