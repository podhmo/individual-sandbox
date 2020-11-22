## やりたい

- sqlalchemy + sqlite + to json + to tabulate
- withの中のコードをASTとして取り出す + eval。 (pdbのsを参考に)
- generate 4 ui from metashape (`cat xxx | cmd`, repl, cli, json-rpc)
- thing at point JSON (encodeのschemaのやつを参考に)
- openapi toolkit

やらなくちゃ

- blog書かなくちゃ カルマ
- advent calendar
- M-xの壊れたshell連携を復活させる

## python ui

- cli
- cat stdin
- repl
- rpc

### python repl

- https://docs.python.org/ja/3/library/cmd.html
- https://docs.python.org/3/library/readline.html
- https://docs.python.org/3/library/rlcompleter.html#module-rlcompleter

### 追記

なぜかreadlineモジュールは上手く動くけど。補完がきかないな。
- https://stackoverflow.com/questions/7116038/python-tab-completion-mac-osx-10-7-lion

これか。

```python
import readline
import rlcompleter
if 'libedit' in readline.__doc__:
    readline.parse_and_bind("bind ^I rl_complete")
else:
    readline.parse_and_bind("tab: complete")
```

### hmm

結局prompt toolkitを使った方ら楽なのでは？
https://python-prompt-toolkit.readthedocs.io/en/master/

## python withの中でAST

- pdbのset_trace()を見る
- なんかsys.auditも実行されている
- sys.settrace()が鍵？, sys.gettrace()

  - https://docs.python.org/ja/3/library/sys.html#sys.settrace
- bdbというモジュールもあるのか。。generic debugger

### ast

- https://greentreesnakes.readthedocs.io/en/latest/tofrom.html

## python rich

なんかwidthの表示がおかしい？
どうもscreenから実行するとそうなるようだ。

### 追記

tmuxだとどうだろう？大丈夫そう

### tmux

.tmux.conf

```
set -g prefix C-j
unbind C-b

# split window
bind | split-window -h
bind - split-window -v
bind S split-window -v

# move in pane (not worked..)
bind-key C-Tab select-pane -t :.+
bind-key C-S-Tab select-pane -t :.-
bind-key C-S-q break-pane
# bind-key C-x

# copying and pasting
bind [ copy-mode
bind ] paste-buffer -s \015

# focus pane
set -g pane-border-style 'fg=colour235,bg=colour238'
set -g pane-active-border-style 'fg=colour51,bg=colour236'

# mouse on
set -g mouse on
```

こんな感じでこれまでと同様に使えそう。

### sessionのattachは?

```console
$ tmux a -t 1
```

### 設定のreloadは？

```console
$ tmux source-file ~/.tmux.conf
```

## python sqlalchemy

いろいろ復習

- https://docs.sqlalchemy.org/en/13/core/tutorial.html
- 1.4からだいぶ変わるらしい https://docs.sqlalchemy.org/en/14/changelog/migration_20.html
- richでtable表示ができる
