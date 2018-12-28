(let ((xs (loop for c in (charset-list)
                when (string-match-p "jis" (symbol-name c))
                collect c
                )))
  (insert "\n")
  (dolist (x xs)
    (insert (format ";; - %S\n" x))))
;; - katakana-sjis
;; - japanese-jisx0213\.2004-1
;; - japanese-jisx0213-a
;; - japanese-jisx0213-2
;; - japanese-jisx0213-1
;; - japanese-jisx0212
;; - japanese-jisx0208-1978
;; - japanese-jisx0208
;; - katakana-jisx0201
;; - latin-jisx0201
;; - jisx0201
