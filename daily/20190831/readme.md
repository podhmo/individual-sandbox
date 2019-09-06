## go functional optionsを良い感じに使いたいな

compile errorにしたい

- [../20190826/readme.md](../20190826/readme.md)

## emacs goでのcompletionをivyのinterfaceにしたい

現在は completion-at-point をそのまま使っている。
これは completion-at-point-functions あたりをみている？

goではこれ

- eglot-completion-at-point

どうして ivy-postframeが動いていないんだろう？

```lisp
  ;; Notice: not in ivy-mode, the setting for display-function is not activated, yet
  ;; (when calling ivy-posframe-display-at-point, and the help message is displayed for current cursor symbol, then move to this)
  (use-package ivy-posframe
    :ensure t
    :commands (ivy-postframe-enable)
    :after ivy
    :config
    (setq ivy-display-function nil) ; default
    (add-to-list 'ivy-display-functions-alist '(complete-symbol . ivy-posframe-display-at-point))
    )
  )

;; ここからは github のreadme

(require 'ivy-posframe)
;; Different command can use different display function.
(setq ivy-posframe-height-alist '((swiper . 20)
                                  (t      . 40)))

(setq ivy-posframe-display-functions-alist
      '((swiper          . nil)
        (complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
        (t               . ivy-posframe-display)))
(ivy-posframe-mode 1)
```

ivy-postframeを外す方法がわからなくなってしまった。

## emacs eglotの通信をcurrent bufferのみに絞り込みたい

## go type XXX map[string]string などってrangeができるの？

- https://golang.org/ref/spec#For_statements

特にかいていないけれどunderlying typeを見る？

## go -ldflags "-X <package name>.<variable name>=<value>"

あとrevisionは `git rev-parse --verify HEAD`

## go echo

wrap handler系のことこの辺りに書いてあるのか

- https://qiita.com/mashiro/items/04024ad755c1148189d7
- https://echo.labstack.com/guide/error-handling

### test

- https://echo.labstack.com/guide/testing
