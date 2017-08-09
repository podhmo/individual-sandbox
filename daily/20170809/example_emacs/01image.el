;;
(add-text-properties (point-min) (+ 1 (point-min))
                     `(display ,(create-image "./a.png")
                               modification-hooks (iimage-modification-hook)))
(remove-text-properties (point-min) (+ 1 (point-min))
                        '(display modification-hooks))

;; from file
(insert-image (create-image "a.png"))


;; from base64
(let ((filename "a.png.base64.txt"))
  (let ((outname "/tmp/a.png"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents filename)
      (base64-decode-region (point-min) (point-max))
      (image-type-from-buffer)
      (write-file "/tmp/a.png"))
    (insert-image (create-image outname))
    ))

;; detect image type
(image-type "a.png")
(init-image-library (image-type "a.png"))
