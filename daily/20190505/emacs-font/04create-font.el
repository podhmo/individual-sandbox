;; (describe-function 'create-fontset-from-fontset-spec)

;; (mapcar #'fontset-plain-name (fontset-list))
;; => ("notosansgothic" "auto1: 23-dot" "startup: 17-dot" "standard: 16-dot medium" "default")

(create-fontset-from-ascii-font "Noto Sans CJK JP:style=Regular:slant=normal:weight=normal" nil "NotoSansGothic")

(set-fontset-font "notosansgothic" 'unicode)

(x-resolve-font-name "notosansgothic")

(when (ignore-errors  (x-resolve-font-name "notosansgothic")))
(set-fontset-font nil 'unicode "Noto Sans CJK JP:style=Regular:slant=normal:weight=normal:size=22")
;; (set-font-encoding)

;;	fontspec FONTSET-NAME[,SCRIPT-NAME0:FONT-NAME0,SCRIPT-NAME1:FONT-NAME1] ...
(create-fontset-from-fontset-spec "")
