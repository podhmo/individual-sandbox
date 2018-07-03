## emacs jediの補完に必要なvenvは１つだけでは？

## python defaultで入っていて欲しいもの

```console
$ pip install dictknife[yaml] kamidana flake8 --user
```

## python -m venvってpip無い時作ってくれない？

ensurepip使うか

```console
$ python -m ensurepip --user --default-pip
$ which pip
$HOME/.local/bin/pip
```

## python venvの再生成

```console
$ python -m venv --clear <path>
```

:warning: 本当に全部消えるのでやばい

## emacs jedi-core

M-x `jedi:install-server` が推奨されている。これを使うとpython-environment.elで作った環境にインストールされる。

`jedi:server-command` が変更される(setq-default)

```lisp
jedi:server-command; => ("python" "$HOME/venvs/my/emacs-sandbox/emacs.d/.cask/25.3/elpa/jedi-core-20170121.610/jediepcserver.py")
(jedi:install-server)
jedi:server-command; => ("$HOME/.emacs.d/.python-environments/default/bin/jediepcserver")
```

## emacs python-environment.el

- defaultで ~/.emacs.d/.python-environments/defaultにvenvを作る
- virtualenv入っていない環境だとエラーになる？
- venv使っても良い気がした

venv

```lisp
(custom-set-variables
 '(python-environment-virtualenv (list "python" "-m" "venv" "--system-site-packages")))
```

どう実行されるか試す

```lisp
;; 存在しなかったらvenvを作成
(unless (python-environment-exists-p)
    (python-environment-make-block))

;; 作成される位置
(python-environment-root-path nil); => "$HOME/.emacs.d/.python-environments/default"

;; 作成されたpythonを使う時に
(let ((cmd '("python" "-V"))
      (root nil))
  (cons (python-environment-bin (car cmd)) (cdr cmd)))
; => ("$HOME/.emacs.d/.python-environments/default/bin/python" "-V")

;; 実際に使う
(deferred:$
  (python-environment-run
   (list "python" "-c" "'import sys; import os; print(sys.executable.replace(os.environ[\"HOME\"],\"$HOME\"))'"))
  (deferred:watch it (lambda (o) (print (list "ok" o)))))
;; 標準出力は取れないみたい
```


## emacs lisp 良い感じのmasking

```lisp
(defvar my:masking-buffer-candidates-alist
  `(
    (,(lambda ()
        (when-let ((it (pickup:pickup-file "/bin/pip")))
          (replace-regexp-in-string "/bin/pip$" "" it)))
     . "VENV")
    (,(getenv "GOPATH") . "$GOPATH")
    (,(getenv "HOME") . "$HOME")
    ))

(defun my:masking-buffer (beg end)
  "あんまり見せたくないpathなどをmaskする"
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min))
    (setq end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (cl-loop
       for (pattern . replacement) in my:masking-buffer-candidates-alist
       do
       (progn
         (when-let ((it (if (functionp pattern) (funcall pattern) pattern)))
           (message "MASKING-APPLY %s %s" it replacement)
           (goto-char (point-min))
           (while (search-forward it nil t 1)
             (replace-match replacement nil t))))))))

```
