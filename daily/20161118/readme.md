# golang gomockのこと

以下が必要な感じ

```
go get github.com/golang/mock/gomock
go get github.com/golang/mock/mockgen
```

# emacs revert-buffer

以下の様な感じ

```lisp
; M-x revert-buffer
;revert-buffer -> calling (funcall (or revert-buffer-function #'revert-buffer--default) ..)
;そしてbefore-revert-hookとかafter-revert-hookとか存在する

(defun yay (&rest args) (message "yay %s" args))
(add-hook 'before-revert-hook 'yay)
;(remove-hook 'before-revert-hook 'yay)
```

