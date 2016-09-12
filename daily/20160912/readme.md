# golang emacs emacs上で `$GOPATH` の値を利用したい場合

以下のような設定を利用する。(.bash_profileで設定しているならloginを使わないとダメ)

```lisp
(let ((output (shell-command-to-string "$SHELL --login -i -c 'echo $GOPATH'")))
  (setenv "GOPATH" (car (last (split-string output)))))
```

# git go privateのrepositoryを `go get` できるようにする

sshでpushできるようにして、 `~/.gitconfig` に以下を加える。

```
[url "git@github.com:"]
  insteadOf = https://github.com/
```

# git githubのoriginの設定書き換え

```bash
git remote remove origin
git remote add origin git@github.com:podhmo/individual-sandbox.git
git push --set-upstream origin master
```

# mac カーソルの移動をもう少し早くする

- https://pqrs.org/osx/karabiner/index.html.ja

key repeat タブ選ぶ。

- Delay until repeat 350
- Key repeat 33

こういうの単に設定ファイルの書き換えで済ませられないのかな？

# emacs mac emacsの設定

個人的にmacportsを使っている。

```bash
$ sudo port install emacs emacs-app
$ mkdir ~/work; cd ~/work
$ git clone https://github.com/podhmo/emacs-sandbox.git
$ ln -s ~/work/emacs-sandbox/emacs.d ~/.emacs.d
# cask
$ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
$ echo 'export PATH=~/.cask/bin:$PATH' >> ~/.bash_profile
$ cd ~/.emacs.d
$ cask install --verbose
```

:notebook: そういえば、gui版のemacsはemacs-appだった。


## problemm

`Invalid version syntax: 'DEV'` というエラー。
原因は以下のような感じ。

```
./.cask/24.5/elpa/cask-20160907.306/cask.el:If DEV-MODE is true, the dev template is used, otherwise the
./.cask/24.5/elpa/flymake-easy-20140818.55/flymake-easy.el:;; Version: DEV
./.cask/24.5/elpa/flymake-ruby-20121104.1059/flymake-ruby.el:;;; Version: DEV
./3rdparty/flymake-easy-20140818.55/flymake-easy.el:;; Version: DEV
./3rdparty/flymake-eslint-0.0.0/flymake-eslint-pkg.el:(define-package "flymake-eslint" "DEV" "no description" (quote nil))
./3rdparty/flymake-eslint-0.0.0/flymake-eslint.el:;;; Version: DEV
```

versionがないやつがある。これがインストールされてしまって結果として死ぬ。

## 雑な対応

```
$ sudo port install gsed
$ grep -rl --include='*.el' DEV . | xargs gsed -i 's@DEV@0.0.0@g'
```
# terminal mac terminalの設定

とりあえず以下だけあれば良い。

.screenrc

```
escape ^Jj
autodetach on
defscrollback 3000
vbell off
hardstatus alwayslastline "%{= rw} %H %{= wk}%-Lw%{= bw}%n%f* %t%{= wk}%+Lw %{= wk}%=%{= gk} %y/%m/%d %c "
```

.bash_profile

```
export PATH=~/.cask/bin:/opt/local/bin:$PATH

# go
export GOPATH=~/work/go
export PATH=$GOPATH/bin:$PATH

# short hand
alias sandbox="cd ~/work/individual-sandbox/daily"
alias e="emacsclient -q -n"
```

# go wip goの環境

GOPATHの設定とかもしないとか。

```
$ sudo port install go
$ go get -u golang.org/x/tools/cmd/godoc
$ go get -u github.com/golang/lint/golint
$ go get -u github.com/nsf/gocode
$ go get -u github.com/rogpeppe/godef
$ go get -u github.com/podhmo/selfish/cmd/selfish
```
