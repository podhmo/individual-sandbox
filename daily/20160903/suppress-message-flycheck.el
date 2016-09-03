(defun flycheck-buffer-status-callback (syntax-check)
  "Create a status callback for SYNTAX-CHECK in the current buffer."
  (lexical-let ((syntax-check syntax-check))
    (lambda (&rest args)
      (apply #'my:flycheck-report-buffer-checker-status
             syntax-check args))))

(defun my:flycheck-report-buffer-checker-status
    (syntax-check status &optional data)
  (flet ((message
          (format-string &rest args)
          (let* ((s (apply #'format format-string args))
                 (lines (split-string s "\n")))
            (princ (first lines)))))
    (flycheck-report-buffer-checker-status syntax-check status data)))

;; activate
(defalias 'flycheck-buffer-status-callback 'my:flycheck-buffer-status-callback)
;; (setq debug-on-error t)
