;;; package --- highlight todo

;;; Commentary:
;; this minor-mode highlights todo

;;; Settings:
;; (add-hook 'text-mode-hook 'htodo-mode)

;;; Code:

(defvar  htodo-font-lock-keywords `((,(regexp-opt (list "todo" "ToDo" "Todo" "TODO")) . font-lock-warning-face)))

(defun htodo-turn-on ()
  "Unconditionally turn on hightlight todos."
  (font-lock-add-keywords nil `(,@htodo-font-lock-keywords) t)
  )

(defun htodo-turn-off ()
  "Unconditionally turn off hightlight todos."
  (font-lock-remove-keywords nil `(,@htodo-font-lock-keywords))
  )

;;;###autoload
(define-minor-mode htodo-mode
  "htodo-mode (minor version)" :lighter " htodo"
  (progn
    (if htodo-mode (htodo-turn-on) (htodo-turn-off))
    (font-lock-mode 1)
    )
  )

(provide 'htodo-mode)
;;; htodo-mode.el ends here
