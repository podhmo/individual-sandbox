(frame-parameter nil 'font);; => "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"
(call-interactively 'describe-fontset)

(fontset-font t 12354);; => (nil . "jisx0208*-*")
(fontset-font t ?あ);; => (nil . "jisx0208*-*")
