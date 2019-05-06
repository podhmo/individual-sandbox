## emacs emojiをsymbolaからnotoの方に変える

  :dizzy: 上手くいかない

```
(fontset-font nil (string-to-char "a"))
(fontset-font nil ?a)

(fontset-font nil (string-to-char "💭"))
(fontset-font nil ?💭)

(set-fontset-font nil ?💭
  (font-spec :family "Noto Color Emoji") nil 'prepend)
(setq use-default-font-for-symbols nil)
```
## emacs fontを調整

- [../20181224/readme.md](../20181224/readme.md)

```lisp
;; 現在利用できるのfontsetを確認する
(call-interactively 'list-fontsets)

;; 現在利用しているfontsetを確認する
(frame-parameter nil 'font) 
"-ADBO-Source Code Pro-normal-normal-normal-*-23-*-*-*-m-0-iso10646-1"

;; 現在のfontsetについて詳しく見る
(describe-fontset nil)

;; fontset-specの情報の表示
;; (fontset-info (car (fontset-list)))
```

まじめに設定するか

```lisp
(let ((fontset-plain-name "notosansgothic")
      (when (ignore-errors (x-resolve-font-name fontset-plain-name))
        (set-fontset-font nil 'unicode "Noto Sans CJK JP:style=Regular:slant=normal:weight=normal:size=22")
        )
))

(set-fontset-font nil 'unicode
(font-spec :family "Noto Sans CJK JP" :weight 'normal :slant 'normal :style "Regular" :size 22))
```

### 使えそうなfontを調べる

```console
$ fc-list | grep -i noto | grep -i gothic
/usr/share/fonts/noto/NotoSansGothic-Regular.ttf: Noto Sans Gothic:style=Regular
```

emacs側からだとこう？

```lisp
(let ((case-fold-search t))
  (loop for x in (font-family-list)
        when (string-match-p "gothic" x)
        do (insert (format ";; => %s\n" x)))
)
;; => URW Gothic
;; => Noto Sans Gothic
```

いや ちがう

```lisp
(error "No fonts match ‘noto sans gothic:pixelsize=18:slant=normal:bold:normal’")
```

こういう感じで探すべき

```
;; consoleでfc-matchで探した

(x-resolve-font-name "Noto Sans CJK JP")
;; => "-GOOG-Noto Sans CJK JP-light-normal-normal-*-*-*-*-*-*-0-iso10646-1"

(x-resolve-font-name "*")
;; => "-GOOG-Noto Serif CJK TC-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"

(x-resolve-font-name nil)
;; => "-ADBO-Source Code Pro-normal-normal-normal-*-23-*-*-*-m-0-iso10646-1"
```


```lisp
(create-fontset-from-fontset-spec "")
```

## memo

- emacsで日本語入力中に文字が表示されない(確定後ようやく表示される)
- emacsのfontが気になる
- 一時的にbluetoothキーボードが使えなくなる(rfkillでも死ぬ)

