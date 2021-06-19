## go 欲しいもの

- JSONError (errorがJSONのもの)
- graphqlのtypeをいい感じに生成してくれるもの
- いい感じにstoreにアクセスする機構
- Argumentをbindする何か

## graphql

gqlgenを使いたくない理由は何なんだろうな？

- https://github.com/graphql-go/graphql

なるほど、雰囲気はわかった。schemaを定義する感じで進めていく。
routerはschema定義ということになる。queryとmutationをmountするイメージ。

次は、paginationの実装だけでもやってみると良いのかもしれない。
あと、テスト？

### 追記

graph-gophers/graphql-goというのもあるのか

- https://github.com/graph-gophers/graphql-go
- https://github.com/samsarahq/thunder

## 使用感

- handlerというよりhandler factory的な実装になるのが自然そう。

  - この辺はgoでweb APIを書いているときにも感じられることだった

- typeがある意味query builder的な感じで作るのが面倒かもしれない。

  - そういう意味では、型定義から生成されて欲しさは感じるかも。

### どこが変わる？

REST APIと良いとこ取りをする感じにしたいのだよなー。

- paramsは全部変わりそう。graphql.ResolveParams
- responseはどうだろう？変わらないかと思いきやedgesがどうこうとか考えないとダメか。
- やっぱり基準はinteractorあたりという気はする。

  - そう考えると、controllerの自作がだるいけれど、仕方がない。
  - endpoint毎に実装を書くならREST APIでも変わらなくない？

- 型定義を自前で書く必要があるのがだるいかもしれない。

### graphqlに絞るとしてどういう構成にする？

- queryとmutationで分けたくないかも？
- やるなら `<domain>/query.go` と `<domain>/mutation.go` を大量に作ってmount

### 知りたいことある？

- gqlを出力する方法ってどうやるんだろ？
- types

  - nullable以外の定義ってどうするんだろ。strictにする方法。

    - [NewNonNull()でwrapする感じ](https://github.com/graphql-go/graphql/blob/master/testutil/testutil.go#L125)

  - enumってどうするんだろ？

    - [NewEnum()がある](https://github.com/graphql-go/graphql/blob/master/testutil/testutil.go#L101)
    - 基本的に、testutilパッケージを覗いてあげれば良さそう。

- httpなしで直接つなぐのはどうしたら良いんだろう？

  - graphql.Do()っぽいな。

- store的なものにアクセスするのはどうすれば良いんだろう？

  - Do()にわたすRootObjectとresolverの中でのSource (untypedだけど仕方がない？)

- 引数に触るのはどうすれば良いんだろう？

  - https://github.com/graphql-go/graphql/blob/master/examples/modify-context/main.go

- dataloader?

  - https://github.com/graph-gophers/dataloader
  - gqlgen用? https://github.com/vektah/dataloaden
  - どうやら、contextのloadersと言う名前で詰め込んでいる

```go
				thunk := loaders["GetCategory"].LoadMany(p.Context, keys)
				return func() (interface{}, error) {
					categories, errs := thunk()
					if len(errs) > 0 {
						return nil, handleErrors(errs)
					}
					return categories, nil
				}, nil
```


- interface, union, fragments
