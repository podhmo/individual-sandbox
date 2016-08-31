;; require rlet1, anything, popwin

(defun my:anything-godoc--read-query ()
  (rlet1 r (anything-comp-read "godoc; "
                               (go--old-completion-list-style (go-packages))
                               :history go-godoc-history)
    (push r go-godoc-history)))

(defun my:godoc ()
  (interactive)
  (let ((query (my:anything-godoc--read-query)))
    (go--godoc query godoc-command)))


(defun my:godoc--get-buffer (query)
  "Get an empty buffer for a godoc query."
  (let* ((buffer-name "*godoc*")
         (buffer (get-buffer buffer-name)))
    ;; Kill the existing buffer if it already exists.
    (when buffer (kill-buffer buffer))
    (get-buffer-create buffer-name)))


;; ;; settings
;; (eval-after-load "go-mode"
;;   '(progn

;;      (defalias 'godoc 'my:godoc)
;;      (defalias 'godoc--get-buffer 'my:godoc--get-buffer)

;;      ;; popwin
;;      (when (boundp 'popwin:special-display-config)
;;        (push '("^\\*godoc [^ ]+\\*$" :regexp t) popwin:special-display-config)
;;        (push '("*godoc*") popwin:special-display-config)
;;        )
;;      ))
