## python やる気が出たのでopenAPI parserを書く

結局、イチから再実装したほうが良いのかもしれない?

気にしないといけないことリストをここに作っておく？

- schema,arrayのparse
- pathsのparse
- metashape互換の形でアクセス可能にする

hmm

- accessor,resolver,dispatcherの仕組みは今でも使う？
- accessorとresolverを一緒にしたほうが良い？

## go cli

- main

  - parse -- error時にhelp usageを出す
  - run -- error時にpanicなど = setup + Run

- Run

  - 普通の実行

### subcommand

subcommandになるとどうなるか？Run()がひとつなのは変わらない。
どれを見たいかも変わらない。一部情報を共有して進める場合はある。

結局名前によるrouting。flagで絞ったあとの結果で。

### nestしたsubcommand

基本的なスタイルとしては同じか。。
余分な引数をどうやって扱うかという話そう。

パッケージ毎にRootOptionを持てば良いだけ。
