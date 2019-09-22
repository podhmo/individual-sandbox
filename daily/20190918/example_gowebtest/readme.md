# go go-webtest

昨日はコレを書いた。

- https://pod.hatenablog.com/entry/2019/09/18/000722

全部答えられているわけじゃないのだけれど。まぁとりあえず。

## 次に何をしようか

もう少し実際の例を扱いたい感じはする。hello world的なAPIを触らずに。

一応後で気にしたいことは以下（但し今回は気にしない予定）

- dbのautoincrementの初期値をランダムに
- testの実行順序をランダムに

## テスト対象を決める

この辺がけっこう良いかもしれない。

https://github.com/deepmap/oapi-codegen/tree/master/examples/petstore-expanded/chi

もとの[petstore_test.go](https://github.com/deepmap/oapi-codegen/blob/master/examples/petstore-expanded/chi/petstore_test.go)の代わりにgo-webtestを使った形でコードを書いてみる。

気づいたことをメモしていきたい

### 追記

気づいたことは以下

- やっぱりJSONを`interface{}`を入力として与えたいかもしれない
- データのセットアップの部分がコードの大部分を占める
- tryパッケージを使っていたとしても明示的にresponseを取り出したいかもしれない
- けっこう丁寧にサブテストを作っていかないとダメかもしれない？
- （snapshot testingを使わない部分はまぁサブテストにする必要もなかったりはするのだけれど）
- テーブルドリブンテストにしても良いような気がする？
- テスト名をガチャガチャと変えたときのsnapshotの扱いがちょっとめんどう？
- 可変長引数を取ってsliceを作る関数が欲しいかもしれない

テスト自体の書き方の話

- APIのグループ毎にテストを書きたいかもしれない
- テスト関数を分けたいとおもったときには `*testing.M` のお世話になりそう
- mock用のmapを露出して設定するのはたしかにサンプル用のアプリのコードとしては便利そう
- 闇雲にテーブルドリブンテストにするとコードが増える
- setup,teardownのペアにテストのときにもdeferを使うのはありかもしれない
- 素直にopenAPIと同様にpath/method/statusで区切った方が良かったかもしれない
