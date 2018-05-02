#[golang]goでは自分自身を返すメソッドは定義しないほうが良いのでは？という話。

最近色々コードを書いていた中で、goでは自分自身を返すメソッドは定義しないほうが良いのでは？という思いを抱くことがあった。自分自身を抱えすメソッドを定義するということを一般に悪いとする主義主張の人もいたりもするけれど。そこまで強硬に主張したいわけではなく。あくまでgoを書く上ではだけの話。

## 自分自身を返すメソッド?

具体的には、ElasticSearchにアクセスする必要があり以下のライブラリを使っていた。

- https://github.com/olivere/elastic

このライブラリで対象のindexに対して検索を行う機能がありそれには以下のようなstructを使う。(正確に言うと10000件以内の検索であればSearchServiceで十分だがそれ以上の件数であればScrollServiceを使う必要がある。いわゆるcursor的なもの)

```go
// ScrollService iterates over pages of search results from Elasticsearch.
type ScrollService struct {
	client            *Client
	retrier           Retrier
	indices           []string
	types             []string
	keepAlive         string
	body              interface{}
	ss                *SearchSource
	size              *int
	pretty            bool
	routing           string
	preference        string
	ignoreUnavailable *bool
	allowNoIndices    *bool
	expandWildcards   string

	mu       sync.RWMutex
	scrollId string
}


// Do returns the next search result. It will return io.EOF as error if there
// are no more search results.
func (s *ScrollService) Do(ctx context.Context) (*SearchResult, error) {
	s.mu.RLock()
	nextScrollId := s.scrollId
	s.mu.RUnlock()
	if len(nextScrollId) == 0 {
		return s.first(ctx)
	}
	return s.next(ctx)
}
```

このstruct自体に問題があるわけではない。作ったstructの`Do()`というメソッドを呼ぶことでElasticSearchにrequestを投げることができる。これ自体はミドルウェアへのアクセスを提供するライブラリとして一般的な定義ではあると思う。

一方で、structは設定として、pagination時のsizeを指定できたり、やscrollId(cursor)を既に保持している場合に渡せたりということができる。内部的にはunexported fieldなのだけれど。とりあえず色々な設定のためのAPIを持っている。

```go
// Size specifies the number of documents Elasticsearch should return
// from each shard, per page.
func (s *ScrollService) Size(size int) *ScrollService {
	s.size = &size
	return s
}

// ScrollId specifies the identifier of a scroll in action.
func (s *ScrollService) ScrollId(scrollId string) *ScrollService {
	s.mu.Lock()
	s.scrollId = scrollId
	s.mu.Unlock()
	return s
}
```

`Size()`は自分自身の`size`を変更し、自分自身を返す。`ScrollId()`も同様。

実はこれらのAPIが曲者なのでは？というのがこの記事の主題。あるstructの状態(設定)を変更するためのAPIとして自分自身を返すAPIの定義が良いのかどうかという話。

## どういう時に嫌になる？

普通に使う分には問題ない。普通に使うというのは、提供されているAPIを直接使う場合のこと。

```
response, err := scroll.Size(30).Do(ctx)
```

では普通ではない使いかたとはどういうことかというとインターフェイスを切ろうとする場合。定義されている型を直接使うのではなく、自分で定義したような何らかの型から間接的に扱いたいような場合。

### テストを書こうとインターフェイスに切り出そうとする場合

例えばちょっとしたテストを書こうと外部リソースへのアクセスをインターフェイスに切り出すということはよくあることだと思う。例えば以下の記事ではHTTPアクセスに対して自分自身でinterfaceを定義して使う例。

- [Dependency Injection in Golang – Zach Kauffman – Medium](https://medium.com/@zach_4342/dependency-injection-in-golang-e587c69478a8)

雑にまとめると以下のようなインターフェイスを定義してあげるとテストとか書きやすくて楽だよね。という話。

```go
type HttpClient interface {
    Get(string) (*http.Response, error)
}
```

さて、これを先程の自分自身を返すメソッドを持つstructに適用しようとしてみよとすると...できない。定義しづらい。設定の機能を持たない単純なinterfaceは以下の様になる。

```go
// Doしか持たないinterfaceをScrollServiceという風に名前をつけるのはおかしいけれど

type ScrollService interface {
	Do(ctx context.Context) (*elastic.SearchResult, error)
}
```

ここで先程の設定を行うAPIに関する部分もコードで利用してしまっているとする（本来はもう少し複雑なもの。あるいは自分で定義したstructのメソッド）。

```go
func Fetch(ctx context.Context, scroll *elastic.ScrollService, size int) ([]Document, error) {
	response, err := scroll.Size(size).Do(ctx)
	if err != nil {
		return nil, err
	}

    var docs []Document

    // responseを良い感じにparseして[]Documentを得る
    ..

	return docs, nil
}
```

このとき先程のinterfaceではコンパイルが通らない。`Size()` も使っているので。

それでは自分自身を返すメソッドも要求してみることにしよう。

```go
type ScrollService interface {
	Do(ctx context.Context) (*elastic.SearchResult, error)
	Size(size int) ScrollService
}
```

このようなインターフェイスで先程の`Fetch()`関数のコンパイルは通るようになる。しかし、大本の`*elastic.ScrollService`はこのinterfaceを満足しない。以下２つは異なるので。

```
func Size(int) ScrollService
func Size(int) *elastic.ScrollService
```

これが問題。goには自分自身の型を表現する型などというものなどは存在していないので。interfaceを切るときにどうしても元の実装への依存を要求してしまう。もし仮に自分自身を返すのを止めているのならばinterfaceを定義することができた(`SetSize()`という名前の方が妥当かもしれない)。

```go
type ScrollService interface {
	Do(ctx context.Context) (*elastic.SearchResult, error)
	Size(size int)
}
```

もちろん元のコードの `scroll.Size(size).Do(ctx)` の部分は２行にわけられて書かれるようになる。

と、まぁ、そんなわけで自分自身を返すAPIというのは良い案ではないのではないかと感じている。なんというか、利便性を求めて生やしたAPIが、ちょっと凝った使いかたをしようとしたときに(e.g. テストのときに)重荷になってしまっていると表現しても良いような状態になってしまっている感じ。

## 問題への対応

問題への対応もワークアラウンド的なものは考えられなくはない。

### wrapしたstructを定義

１つは元のstructをwrapしたstructを定義すること。このようにしてあげれば自分で定義した戻り値無しのinterfaceを満足する実装になる。

```go
type myScrollService struct {
	*elastic.ScrollService
}

func (s *myScrollService) Size(size int) {
	s.ScrollService.Size(size)
}
```

しかし、正直な所、このようなwrapperを毎回書くのは馬鹿馬鹿しい。

### 利用する部分で要求するinterfaceを小さくする

あるいは利用する部分で要求するinterfaceを小さくすることもできるかもしれない。これは先程のFetch関数を以下のように２つの関数に分離するということ。

```go
func Fetch(ctx context.Context, scroll *elastic.ScrollService, size int) ([]Document, error) {
	return fetch(ctx, scroll.Size(size))
}

func fetch(ctx context.Context, scroll ScrollService) ([]Document, error) {
	response, err := scroll.Do(ctx)
	if err != nil {
		return nil, err
	}

    var docs []Document

    // responseを良い感じにparseして[]Documentを得る
    ..

	return docs, nil
}
```

そしてテストのときには `Do()` だけを要求するinterfaceとして`fetch()`に対してテストを書く。これもこれでテストのために面倒な実装を要求するという点で微妙な感じ(とは言え、元の実装を変えないという意味では一番マシなような気もしている)。

### 元の実装のへの依存を要求することを我慢する

あとは、元の実装のへの依存を要求することを我慢する。諦めてインターフェイスを無理やり定義してしまう方法。一番実装コードの変更が少ないものの。こちらはコンパイルは通るもののテストでエラーみたいな形になって利用するときの精神的コストが幾分か高めになりつらそうな気がしている。

このときのインターフェイスは以下のようなもの。

```go
type ScrollService interface {
	Do(ctx context.Context) (*elastic.SearchResult, error)
	Size(size int) *elastic.ScrollService
}
```
