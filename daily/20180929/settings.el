(setq display-buffer-alist nil)
(add-to-list 'display-buffer-alist `("\\*shell\\*"  . ,display-buffer--same-window-action))

(let* ((filename "./hello.txt")
       (value (find-file-noselect filename nil nil nil)))
  ;;(switch-to-buffer value)
  (pop-to-buffer-same-window value)
)

;; hmm
;;  (setq display-buffer-function 'popwin:display-buffer)
(setq display-buffer-function nil)
