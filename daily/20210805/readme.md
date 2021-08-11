#

## go astmpl 引数の受け取り方

引数の受け取り方は２種類くらい考えられる

- constかvarなどで定義を宣言
- 置き換え対象の関数に余分に引数を追加する

### 追記

重要なこと

- template functionもビルドしたい
- template functionは実行しない
- template functionから生成した定義を利用したコードは書きたい
- template functionのPATHを指定に失敗したくない

## go astmpl marker

marker的なものの実装をどうしようか？
うまく書き換えられるようにしておきたいと言うのと引数を渡したい。
あとは、astからgoのコード生成をするときにスキップしたいパラメーター定義などがある。

- 関数を置き換え対象にすることは許すとして、引数をそのまま使うことはできない
- 一方でemitのタイミングで引数を渡したくなる

構造体を明示的にフィールド名を指定せず初期化するやつって引数不足でいけたっけ？
無理そう。何かしらの値が必要そう。

```go
foo := Foo{"foo", nil}
```

## go astmpl rename-symbol

- 関数名などを書き換えたい場合がある？

```go
func Handler(w http.ResponseWriter, req *http.Request){
    ...
}
```

- とりあえず、関数名だけを書き換えられるようにするか