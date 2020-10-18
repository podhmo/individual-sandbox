## go code生成

いろいろな方法を試してみていた

- 素直にtext/template
- astの変換
- astからtemplateを作成

一番最後が筋が良い気がしている

- Identを利用するというhack
- TypeName()をまともにしたい
- ディレクトリ単位で一気に生成したい

## go structgen

notionのやつを作っててstructを表示してほしかった。
ところで無限再帰してしまうっぽい。

```
path :: [ * struct.BlockRecords slice[0] * struct.Activity * struct.Edits slice[0] struct.BlockData struct.BlockValue struct.TableViews slice[0] * struct.CollectionView * struct.Format * struct.TableProperties slice[0] *]

rtypes :: [*notionapi.Page notionapi.Page []*notionapi.Record *notionapi.Record notionapi.Record *notionapi.Activity notionapi.Activity []notionapi.Edit notionapi.Edit struct { BlockValue notionapi.Block "json:\"block_value\"" } notionapi.Block []*notionapi.TableView *notionapi.TableView notionapi.TableView *notionapi.CollectionView notionapi.CollectionView *notionapi.FormatTable notionapi.FormatTable []*notionapi.TableProperty *notionapi.TableProperty notionapi.TableProperty]
panic: x
```

直したderefがcache効いていなかった。structgenで難しいのはpackage pathの扱いかもしれない。

## go notion

- https://github.com/kjk/notionapi

この辺を参考にしてみるのが良さそう。

- download
- upload

## go client library

もう少し考えてみる。やっぱりclient libraryの自作がめんどくさい。
生成されてほしい。
と、考えてみたのだけど、goaとは何が違うんだろうか？

glueという言葉が適切かもしれない。考えてみると、HTTPのmethodとpathもoptionalかもしれない。

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
