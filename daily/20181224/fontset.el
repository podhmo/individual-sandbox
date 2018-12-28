(progn
  (defvar myfont "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default")
  (set-fontset-font myfont
                    'japanese-jisx0208
                    (font-spec :family "Hiragino Maru Gothic Pro")) ;; font

  (setq default-frame-alist
        `(
          (frame-cursor-color . "steelblue")
          (scroll-bar-background . "grey75")
          (scroll-bar-foreground)
          (border-color . "black")
          (cursor-color . "Steelblue")
          (mouse-color . "gold")
          (background-color . "dark slate gray")
          (foreground-color . "white")
          (font . "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-fontset-startup")
          ,@default-frame-alist))
  )
(call-interactively 'make-frame-command)
