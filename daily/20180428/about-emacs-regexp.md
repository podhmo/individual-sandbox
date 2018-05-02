#[emacs][memo] emacsで正規表現で置換する場合に入力する値のメモ

emacsで正規表現で置換する場合のメモ。ついついescapeなどの対応関係を忘れてしまうので。

例 `int(xxx)`を`xxx`に変換する

`M-x replace-string`でminibufferで入力する場合

```
int(\([^)]+\)) -> \1
```

ちなみに文字列で渡す場合には全部にescapeが要る。

```lisp
(replace-regexp "int(\\([^)]+\\)" "\\1")
```

`M-x re-builder`で`C-c C-w`で保存できるのは後者の形式

