## browse-githubをまともにするか

- `(project-current)` 使っても良いのでは？
- 現在のbranchの現在のファイルを開きたいかも？
- `current-branch` は `(vc-git-branches)` の先頭
- 現在の行は`(line-number-at-pos)`

## python black emacs

blackに変えるか

### 標準入力を渡す方法がわからない

```
$ cat <file> | black -
```

### 関数定義の引数部分の末尾のコンマの扱い

https://gist.github.com/podhmo/36019686c1a6ff10bef17645a7be841f

## slack 豪華なinput

- stateful message
- dialog
- menus

- https://api.slack.com/docs/message-buttons
- https://api.slack.com/interactive-messages
- https://api.slack.com/dialogs

## python discordbot

- https://github.com/Rapptz/discord.py

```
git clone git@github.com:Rapptz/discord.py.git
```

設定用のformatを作りたい。
ついでにrewriteを見るか。
