# git githubのoriginの設定書き換え

```bash
git remote remove origin
git remote add origin git@github.com:podhmo/individual-sandbox.git
git push --set-upstream origin master
```

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
$ cask
```

:notebook: そういえば、gui版のemacsはemacs-appだった。

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

# goの環境

GOPATHの設定とかもしないとか。

```
$ sudo port install go
$ go get -u 
```
