(frame-parameter nil 'foreground-color)
(frame-parameter nil 'background-color)

;; hmm
(when (posframe-workable-p)
  (let ((buf (with-current-buffer (get-buffer-create "*me*")
               (erase-buffer)
               (insert "hello world")
               (current-buffer)
               )))
    (posframe-show
     buf
     :background-color (frame-parameter nil 'foreground-color)
     :foreground-color (frame-parameter nil 'background-color)
     :poshandlr #'posframe-poshandler-frame-center
     )))


(when (posframe-workable-p)
  (posframe-show
   "*me*"
   :string "hello"
   :background-color "black"
   :foreground-color "yellow"
   :internal-border-width 2
   :internal-border-color "red"
   )
  )

;; all posframe buffer
(dolist (frame (frame-list))
  (when-let* ((buffer-info (frame-parameter frame 'posframe-buffer)))
    (print buffer-info) ; (name . buffer)
    ))

;; こうなっているのでふつうに消せる
;; (add-hook 'kill-buffer-hook #'posframe-auto-delete)
;; (kill-buffer (get-buffer "*me*"))

;; frameを消す？frameを隠す？
;; make-frame-invisibleをすれば良い

;; (posframe-delete-all)
;; M-x posframe-delete-all

;; N秒後に消える
(lexical-let ((bufname "*me*"))
  (posframe-show
   bufname
   :internal-border-width 20
   :string "hello!!"
   )
  (run-at-time "3 sec" nil
               #'(lambda ()
                   (kill-buffer (get-buffer bufname))
                   ))
  )

(lexical-let ((bufname "*me*"))
  (posframe-show
   bufname
   :internal-border-width 20
   :string "hello!!"
   :timeout 3
   )
  )

;; 幅の動的な変更はどうすれば良いんだろう？

(dotimes (i 10)
  (lexical-let ((bufname (format "*me:%d*" i)))
    (posframe-show
     bufname
     :string (format "hello: %d" i)
     :x-pixel-offset (* 200 i)
     )
    (run-at-time "3 sec" nil
                 #'(lambda ()
                     (kill-buffer (get-buffer bufname))
                     ))
    )
  )
