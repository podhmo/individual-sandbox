# go droplets

- https://github.com/spy16/droplets

そもそも構造が分かっていない。

## 構造

ディレクトリ構造のドキュメントがあるらしい

- https://github.com/spy16/droplets/blob/master/docs/organization.md

ただ間延びして読みづらい。

- domain/
- usecases/
- interfaces/
- pkg/
- web/

なるほど。web/はassetsでpkg/はlib的なものっぽい。
なので見るべきはdomain,usecases,interfaces。
けっこう初歩的なというか基礎的な分け方なきがする。大変なのはこれがネストしたとき？そしてそれぞれが共通部分を持ちはじめたとき？

まぁいいや。

- domainにentitiesが含まれる
- domainは基本的には何の依存も持ち込ませない
- interfaceをどちら側に定義するか辺りが気になることくらい？

## コード

中を覗く

### domain

- SetDefault(), Validate()をメソッドとして持つのか。
- なるほど、まぁ確かにdomainロジックとしてのvalidationは自身を入力にした方が楽かもしれない
- (個人的にはNameなどをMetaにして埋め込みというという定義はあまり好きではない。ただvalidateで渡しやすいかも。interfaceとか定義したくないし)

### usecases

not package, this is directoryというのはわかりやすいのか。

- storeというinterfaceに依存して処理が書かれている
- それぞれのsub packagesがStoreというintefaceを持っている
- retrieverが取得用のそれ
- command的なものはそれぞれstructが生えている。

### interfaces

- mongo/rest/webとある
- mongoはいわゆるインフラ層のもののこと
- restはapplication層
- webはindex.htmlなどdomainにはあまりタッチしないもの
- 特徴的なのは各自のpackageでprivateなinterfaceを定義してそれに依存にしている点
- (interfaceを公開しない。New()の引数にはprivateなinterfaceでもOK)

全部の依存はmain.go()で形作られている。



