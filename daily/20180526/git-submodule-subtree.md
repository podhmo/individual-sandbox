## ipynbをsandbox repositoryに置きたくない

日々の疑問を解決するためのちょっとした試行錯誤をsandboxのrepositoryでやっている。ipynbはサイズが大きくなる傾向があるので含めたくない(ipynbというのはjupyter notebookのやつですね)。

それでもどこかに保存されていると便利ということはあって。何かしらに保存したい。githubにはipynbのpreviewerが付いているのでやっぱり保存されているのは便利なのだけれど。と言うようなことをいろいろ考えていた結果別リポジトリにガンガン保存していけば良いじゃんということになった。

### 具体的な話

要件は以下

- sandboxをcloneしたときにはipynbは含まれてほしくない
- (とは言え、特定の場所や操作を行って手元に手軽にcloneしてくることはしたい)

後者があるので、サブディレクトリを.gitignoreして、git initということはやりたいことから外れる。
ここにきて`git submodule`と`git subtree`の内容を理解していないことに気づいた。なので挙動をメモする。

その前に色々準備。

```console
# とりあえず大本のrepositoryを作る
$ mkdir src-repo
$ cd src-repo
$ git init
$ echo hello > readme.md
$ git add .
$ git commit -m "hello"

# remoteのrepository(githubを模したもの)をつくってpushしておく
$ mkdir remote-repo
$ cd remote-repo
$ git init --bare
$ cd ../src-repo
$ git remote add origin ../remote-repo

# 一応remote-repositoryからのcloneを確認
$ (mkdir tmp; cd tmp; git clone ../../remote-repo; cat remote-repo/readme.md); rm -rf tmp;
hello
```

### subtree

subtreeを試す。

```console
# subtree用のrepositoryを作っておく
$ mkdir src-repo-tree
$ cd src-repo-tree
$ git init
$ echo sub-tree > readme.md
$ git add .
$ git commit -m "init"
$ (mkdir ../remote-repo-tree; cd ../remote-repo-tree; git init --bare)
$ git remote add origin ../remote-repo-tree
$ git push -u origin master

# 大本にsubtreeを適用する
$ git subtree add --prefix=repo-tree --squash ../remote-repo-tree master
$ git push orgin

# 確認
$ (mkdir tmp; cd tmp; git clone ../../remote-repo; tree remote-repo)
Cloning into 'remote-repo'...
done.
remote-repo
├── readme.md
└── repo-tree
    └── readme.md

1 directory, 2 files
$ rm -rf tmp
```

subtreeはclone時に埋め込まれているので違いそう。ちなみにコミットにも埋め込まれる(subtree merge)。

```console
$ git log --pretty=oneline
b1b96fc7d1fa0dfa54252ab512929756a187cbcc Merge commit '81a1151d2e021beb8460fa9d32481ac9c6eafe02' as 'repo-tree'
81a1151d2e021beb8460fa9d32481ac9c6eafe02 Squashed 'repo-tree/' content from commit 77b19b3
dcbfdffad69ebbb51e3fa81626457063356418ae hello
```

## submodule

submoduleの方も同様に使ってみる。こちらは管理が面倒で不評という話をよく聞くのだけれど。。

```console
$ git submodule add ../remote-repo-tree repo-submodule
$ git submodule status --recursive

# 確認
(mkdir tmp; cd tmp; git clone ../../remote-repo; tree remote-repo)
remote-repo
├── readme.md
└── repo-tree
    └── readme.md

1 directory, 2 files
$ rm -rf tmp
```

submoduleの方は期待通りに動作している。

### 変更の追随(pull)

pullの方法もメモ

```console
# 変更されているか確認
$ (mkdir tmp; cd tmp; git clone ../../remote-repo-tree; cat remote-repo-tree/readme.md); rm -rf tmp;

# subtreeをpull。普通にmerge commitができる
$ git subtree pull --prefix=repo-tree --squash ../remote-repo-tree master

# submodule
$ (cd repo-submodule; git pull)
$ git submodule update

# もしく
$ git submodule foreach 'git pull'
```

### memo submoduleの削除

```console
$ git submodule deinit <module name>
$ git rm <module name>
```

