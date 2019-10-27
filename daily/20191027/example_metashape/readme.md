# metashape

- https://github.com/podhmo/metashape

気持ちとしてはこの辺のtweetの意識が強いのかもしれない。わかりやすかった。メモしておこう。

lispのS式のうれしさと似ている話。

- https://mobile.twitter.com/podhmo/status/1188066214859300864
- https://mobile.twitter.com/podhmo/status/1188066502836023296
- https://mobile.twitter.com/podhmo/status/1188066835905662977

構成の管理がしたいという話。

- https://mobile.twitter.com/podhmo/status/1188049171506315265
- https://mobile.twitter.com/podhmo/status/1188009883771125760
- https://mobile.twitter.com/podhmo/status/1188021740426776576
- https://mobile.twitter.com/podhmo/status/1188023127780941825

## 追記 今回は分割出力の部分を気にしたい

考えてみると、どうしても必要なのは分割出力かも。先にそちらを見ておくか。
[prestring](https://github.com/podhmo/prestring)で対応してたっけ？

prestring.outputで一部雑に実装していたみたい。
もう少しまじめに出力できるように実装しちゃっても良いかも(prestringに入れるかは別として)。

## 分割出力

本当に起きて欲しい状況は以下。

- ふつうに実行 (分割されて出力される）
- dry runとして実行 (標準出力に結果が分かる感じで出力される)
- dry runとして実行 (生成されるファイルが分かる)
- namespaceないしはtagでグルーピングされた形で実行

コレとは別のオプショナルの要件として以下がある。

- 依存関係を良い感じにして出力。

### 追記

prestring.output.SeparatedOutput を使ってみた。使いにくい。

- prestringと密結合。
- vfs(Virtual File System)とFileのfactoryが変にくっついている
- vfsへの管理が煩雑。
- 手軽に書けない(context managerのinterfaceくらいは用意してほしかった)。
- 何が起きるか実行されるまでわからない

### 追記

vfs付近のライブラリを覗いてみるか。。

- https://github.com/PyFilesystem/pyfilesystem
- https://django-storages.readthedocs.io/en/latest/

django-storageとかはやりたいものそのものではないのだけれど。そういえばそういうものもあったなーくらいの感じ。
pyfilesystemは古臭い感じはしそう。

こっちのほうが新しいやつか。

- https://github.com/PyFilesystem/pyfilesystem2

```
pip install fs
```


### 追記

inmemory fsで書いてcopy_fsすれば良い？
どうにかなるけれど。project部分まで含めてfilesystemの中に入れておかないとダメそうだなー。

### 追記

そういえばdictknifeでmigrationを作っていたときはどうしてたっけ？

- https://github.com/podhmo/dictknife/blob/master/dictknife/swaggerknife/migration.py

ふつうに力で頑張っているな。。

裏側は使っても良い気がした。


### 追記

とりあえずなしでprestring.outputだけ綺麗にするか

- context managerを追加
- module factoryなどを分解(prefixなどの状況がむずかしい)
- cleanupオプションを明示的に作る
- tag的な情報を使う

物理的な構造とは異なる名前のものが作れると嬉しいのだよなー(strategy)。
moduleとの密結合どうしよう？

