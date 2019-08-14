(defun trace-with (f &rest args)
  (with-current-buffer (get-buffer-create "*trace")
    (goto-char (point-max))
    (let ((time (format-time-string "%Y-%m-%dT%H:%M:%S" (current-time))))
      (insert (format "%s with %s\n" time args)))
    )
  (apply f args))

(defun add (x y)
  (+ x y))

(advice-add 'add :around #'trace-with)
(advice-remove 'add #'trace-with)

(add 10 2);; => 12
(add 10 3);; => 13
