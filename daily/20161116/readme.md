# git 最近の作業ブランチの更新の仕方

rebase `--onto` を多用している。

.gitconfigに以下のようなglogをというaliasを登録している。

```
[alias]
    glog = log --graph --date=short --decorate=short --pretty=format:'%Cgreen%h %Creset%cd %Cblue%an %Cred%d %Creset%s'
```

だいたい以下のようなコマンドで済ませている。

```bash
$ git checkout my-feature
$ git checkout -b tmp-feature  # 先端からtmp branch作る
$ git glog  # 自分の作業の開始地点のrevisionを把握 (e.g. xxxxxxxxxx)
$ git checkout develop  # develop branchを確認などの作業(省略可)
$ git rebase --onto develop xxxxxxxxxx tmp-feature
$ git checkout tmp-feature # 上手くマージしているか確認
$ git checkout my-feature
$ git rebase --hard tmp-feature  # 作業ブランチの指すrevisionをtmp-featureのものに変更
$ git br -D tmp-feature
```
