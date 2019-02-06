## jsonkife bundle

json referenceで部分的にbundleしたい

- けっこうむずかしい？(少なくともBundlerの使いかたを工夫しないとダメ)
- bundlerに渡すのはresolver

どれくらいの可能性があるんだろ？

- ref以外を指定
- 同一ファイルのrefを指定
- 異なるファイルのrefを指定

新しいdocumentを作れば良いのか。。(resolver)

### refだけのドキュメントをbundlerに渡した場合

同一ファイル上への$refがあった場合に死ぬ。

### 新しいdocument(resolver)をつくる場合

conflict checkに引っかかる


## dictknife python 久しぶりに作ってきたコマンドの連動を確認してみても良いかもしれない？

swagger2jsonから使うと実は意外と良いのでは？

- swagger2jsonにmergeのコマンドがないな？
- mergeのコマンドはjson reference形式になっているのが良いな？
- (指定に"#/"の形式になっているのが不便？)

## dictknife shape

- jsonpointerで表示したほうが

## dictknife 12factor appについて

環境変数にemitする機能があっても良いのでは？

## dictknife usecase毎にどう使うかがあって良いのでは？

- 色々な設定ファイルのload
