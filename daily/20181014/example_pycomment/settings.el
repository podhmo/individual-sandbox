(lexical-let ((cmd-path (file-name-directory (buffer-file-name))))
  (defun my:py-comment ()
    (interactive)
    (let ((cmd (format "python %s/pycomment.py %s" cmd-path buffer-file-name)))
      (my:execute-formatter-command "python" cmd (point-min) (point-max))))
  )

(define-key python-mode-map (kbd "C-c `") 'my:py-comment)
