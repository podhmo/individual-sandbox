# emacs dired diredで親ディレクトリへの移動を楽にする

`[` に親ディレクトリへの移動をbindしておく。

```lisp
(defun my:dired-setup ()
  (define-key dired-mode-map (kbd "[") 'dired-up-directory)
  )
(add-hook 'dired-mode-hook 'my:dired-setup)
```
