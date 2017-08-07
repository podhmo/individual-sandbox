## python jupyter emacs ein

## python jupyter jupyter_clientを覗く

jupyter_clientをinstallして使えるようになるコマンド

- jupyter-kernelspec
- jupyter-run

### kernelspec

- list
- install, uninstall


### jupyter-run

```
jupyter-run --existing -y --debug --kernel=python
jupyter-run --debug --kernel=python 
```

この辺でkernelのconnectionは知れるっぽい

```
[RunApp] Connection File not found: ~/Library/Jupyter/runtime/kernel-97143.json
[RunApp] Found kernel python3 in ~/vboxshare/venvs/my3/share/jupyter/kernels
[
```
## emacs emacsのbuffer内での画像の表示

- `M-x iimage-mode`

## emacs jupyter notebookをemacsで

- https://github.com/millejoh/emacs-ipython-notebook

```
M-x package-install ein
```

なんかemacs25のときにしか動かないっぽい(https://github.com/millejoh/emacs-ipython-notebook/issues/219)。

```
sudo port selfupdate
port notes emacs-app
port variants emacs-app
sudo port install emacs-app +image-magic
cd ~/.emacs.d
cask -v
```

```lisp
(require 'ein)

; M-x ein:notebooklist-login
; M-x ein:notebooklist-open
```
