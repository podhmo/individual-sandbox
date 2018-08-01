## emacs なぜかfind-fileの挙動が変

全部current-bufferで開いてほしいのだけれど。

```lisp
(defvar my:xxx-target-file-name "memo262.txt")
(display-buffer (find-file-noselect my:xxx-target-file-name))
(display-buffer-same-window (find-file-noselect my:xxx-target-file-name) nil)
(display-buffer-reuse-window (find-file-noselect my:xxx-target-file-name) nil)
```
