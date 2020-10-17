## go http client

本当にほしいのはhttp clientなんだろうか？
ライブラリのある世界。確認するたびにmain.goが必要。
やっぱりCLIもほしくない？


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
