## emacsで正規表現で置換する場合

emacsで正規表現で置換する場合

`M-x replace-string`でminibufferで入力する場合

```
; int(xxx) を xxx に
int(\([^)]+\)) -> \1
```

ちなみに文字列で渡す場合には全部にescapeが要る。

```lisp
(replace-regexp "int(\\([^)]+\\)" "\\1")
```

`M-x re-builder`で`C-c C-w`で保存できるのは後者の形式

## golang return self微妙じゃん？
