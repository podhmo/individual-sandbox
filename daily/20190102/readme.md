## go mock mockソムリエ

https://github.com/podhmo-sandbox/go-mock-sandbox

なんだか使いにくさとか使いやすさに幾つか壁があるような気がする？

- install,setup
- モック用のコードの生成用の設定
- 最初に一回通るコードを書くまでの大変さ
- 壊れたコードを直すまでの大変さ
- モックがテストの主題ではないときの設定の煩雑さ

### DSLがツライ

なぜreflectionが必要になってしまうのか。
なぜ利用方法についてまじめに学習する必要が出てしまうのか。

### testが失敗するようになるとき

- 挙動の変更
- インターフェイスの変更(シグネチャの変更も含む)

リファクタリングで発生するのは後者で、これに対して手軽に不整合を解消していきたい。
(のだけれど、既存のmockライブラリでは上手くいかないように感じる)

### 2つのmock

テストの主題ではないときにはダミーオブジェクトとして扱いたいことがある。
このダミーオブジェクトとしての扱いやすさが異なっている？

