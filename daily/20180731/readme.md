## emacs diredからファイルを開いた時に別のbufferで開かれる

- find-file-other-window が使われている？

dired上では以下の様な形でコマンドがバインドされている

```
f dired-find-file
o dired-find-file-other-window
RET dired-find-file
```

dired-find-fileの挙動がだめ。

split-width-thresholdでもないらいし。find-fileの時点でダメそう。

```lisp
;; これがだめ
(find-file "../gen.sh")

;; 内部でもこれ
(let ((wild-card nil))
    (pop-to-buffer-same-window
        (find-file-noselect "../gen.sh" nil nil wild-card)))

(let* ((norecord nil)
      (wild-card nil)
      (buffer (find-file-noselect "../gen.sh" nil nil wild-card)))
  (pop-to-buffer buffer display-buffer--same-window-action norecord))
```

この変数のせい？　display-buffer--same-window-action

```
display-buffer--same-window-action is a variable defined in ‘window.el’.
Its value is (display-buffer-same-window (inhibit-same-window))

  This variable may be risky if used as a file-local variable.

Documentation:
A ‘display-buffer’ action for displaying in the same window.
```

そもそも display-buffer-alist という変数が現れたらしい。

### emacs display-buffer-alist

popwinの代わりにemacs24からの機能を使ったなにかがあるらしい

- http://emacs.rubikitch.com/shackle/
- https://emacs.stackexchange.com/questions/12747/how-to-conditionally-reuse-the-current-window-to-display-a-buffer

そもそものリファレンス

- https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Action-Functions.html

