(require 'json)
(progn
  (setq filename "../../20160919/tutorial_pyplot/tutorial-preview.ipynb")
  (setq data (json-read-file filename))
  (insert "\n")
  (seq-doseq (row (assoc-default 'cells data))
    (let ((outputs (assoc-default 'outputs row)))
      (when (and outputs (> (length outputs) 0))
        (seq-doseq (output outputs)
          (let* ((data (assoc-default 'data output)))
            (seq-doseq (datum data)
              (when (eq 'image/png (car datum))
                (insert-image
                 (create-image (base64-decode-string (cdr datum)) nil t))
                (insert "\n"))))))))
  1)

