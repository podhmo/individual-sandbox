## go http client

本当にほしいのはhttp clientなんだろうか？
ライブラリのある世界。確認するたびにmain.goが必要。
やっぱりCLIもほしくない？

### 追記

テキトーに実装を書いて共有してみるか。tenukiを使いたい。

### 追記

考えてみると、clientの内部でresponseをparseしていると、相性が悪いのかもなー。tenukiと。
...
そうでもなかった。transportでだけ管理されているのでけっこう無理ができる。

### 追記

何を気にしたかったのだっけ？

- clientを使ってテストが書ける
- clientの実装で楽がしたい
- cliとしても扱いたい(?)

## go functional options

- 普通に定義したのとinterfaceにするのとで違いがある？
- struct自体をfunctional optionsにできるかも？
- あとは、同じ名前のOptionを定義できなくはないか？

キモは、method名を変えること。同じ型にnew typeできること(e.g. OffsetFuncとかも作れる)。

```go
type LimitFunc func (*BaseParams)
func WithLimit(limit int) LimitFunc {
	return func(p *BaseParams) {
		p.Limit = limit
    }
}

func (f LimitFunc) ApplyFoo(p *FooParams){
	return f(p.BaseParams
}
```

## vanilla jsでSPA

知識としては知っているけれど。まぁ手を動かしてみると良いよね。。と言うやつ。

- https://zenn.dev/gagaga/articles/vanilla-spa-yattarude
- https://github.com/itskihaga/vanilla-spa-sample

## go 便利ライブラリ

- https://github.com/google/go-querystring

これ知らなかった。
