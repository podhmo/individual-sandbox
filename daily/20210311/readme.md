## emacs 削除対象のpackageの一覧を取る

```lisp
(insert (format "%s" (package--removable-packages)))

(yaml-mode with-editor which-key volatile-highlights use-package undo-tree transient terraform-mode quickrun posframe pkg-info paredit markdown-mode magit key-chord ivy-rich ivy-posframe init-loader hcl-mode git-commit flycheck epl dash bind-key async)
```
