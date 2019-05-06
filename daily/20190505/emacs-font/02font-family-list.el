;; (dolist (x (font-family-list))  (insert (format "%s\n" x)))

(let ((case-fold-search t))
  (loop for x in (font-family-list)
        when (string-match-p "cjk jp" x)
        do (insert (format ";; => %s\n" x)))
);; => Noto Serif CJK JP
;; => Noto Serif CJK JP
;; => Noto Sans CJK JP
;; => Noto Serif CJK JP
;; => Noto Sans Mono CJK JP
;; => Noto Sans CJK JP
;; => Noto Sans CJK JP
;; => Noto Sans CJK JP
;; => Noto Serif CJK JP
;; => Noto Sans CJK JP
;; => Noto Serif CJK JP
;; => Noto Sans CJK JP
;; => Noto Serif CJK JP

(let ((case-fold-search t))
  (loop for x in (font-family-list)
        when (string-match-p "emoji" x)
        do (insert (format ";; => %s\n" x)))
);; => Noto Color Emoji

