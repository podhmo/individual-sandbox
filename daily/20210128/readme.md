## go

- goのinterfaceの実装ではなく概念的なもの
  - (genericsで変わりそうなこと)
- goの型に対応した処理とその限界。registerFunctionForCustomType。

このあたりをいい感じに説明したいのだけれど。いい感じの文章を書きたい。

### 幾つかのidea

interfaceというのは結局の所、複数のメソッドを束ねたもの（複数の操作を持っているもの）。どういうものから考えるべきかといえば、関数を引数に取るタイプの高階関数。これに対してサブタイピングがしたくなったときに、名前的なものなのか構造的なものなのかが出てくる。

どういうふうに使っていくか？ということも整理していきたいがどうすれば良いんだろう？

外部とのやりとりや仕様を表すわけではない。Javaなどのinterfaceはもっぱら外部とのやり取りの仕様を表すことに意味があるものだけれど。

```
func Commit(save func(interface{}) error) error

type Saver interface {
	Save(interface{}) error

    // なにか他にメソッドを
}

func Commit2(saver Saver) error
```

もう一つ加わるとしたら、インターフェイスはメソッドの定義なので、状態を持ちうる。↑のようなSaveの部分で引数を取る形だけではなく、内部に状態として保持するというようなこともありうる。

```go
type Saver interface {
	Save() error
}

type Impl struct {
	Ob interface{}
	Store Store
}

type (i *Impl) Save() error {
	return i.Store.Add(i.Ob)
}
```

もう一つあった気がしたのだけれど。忘れてしまった。メソッドの定義自体は関数に対してもできるのでアダプター的な挙動を作る事もできる。ここでアダプター的ななにかというのは、ベースの処理を特定のコンテキスト上の処理に持ち上げる的な意味のこと。しかし、アダプターというと引数として使う側でデータの変換をイメージするかもしれない、こちらは処理の変換というか持ち上げなのでliftと言う言葉が適切なような気がする。

```go
func AddTodo(ctx context.Context, todo Todo) ([]Todo, error) {
	...
}

func (f AddTodo) ServeHTTP(w http.ResponseWRiter, r *http.Request){
	...
	result, err := f(ctx, todo)
    ...
}

func (f AddTodo) ServeCLI(options Options){
	...
	result, err := f(ctx, todo)
    ...
}
```

IoCと呼んでいるものが重要なのかというとどうだろう？手軽なインターフェイスの説明は概ね以下の３つくらい？

- io.Reader,io.Writer ...
- テスト (mock, fake)
- コアロジックの依存を減らす (DI, IoC)

repositoryの実装で外部の形状を気にしすぎると辛いみたいな話がちょうどrepositoryの実装に当てはまりそうなきはしている。repository自体はインターフェイスにせずに、repositoryが依存する処理をinterfaceとして扱うのがきれい（なはず）。

```go
// command, query
func AddTodo(s Store){
}

type Store interface {
	ListFromIDs(objs interface{}) error
}
```

### 幾つかのidea２

goでのstructural subtypingの醍醐味は、事前の取り決めが存在しないものを後付で追加できること。
しかし、既存の型への処理を追加しようとした時にそれが無理になる。

```go
type S struct {
	Name string
}

func (s *S) Validate() error{
	...
}

// では、time.Timeに対するvalidationは？
```

↑の例では後付けの嬉しさを語れていないな。。

### generics?

戻り値や引数が個別に型を取るような形はとてもだるい。

```go
func (s *XStore) Load() []X { ... }
func (s *YStore) Load() []Y { ... }

txpe XLoader interface {
	Load() []X
}
type YLoader interface {
	Load() []Y
}
```

これを１つにまとめたいときに`interface{}`がある。JSONなどがそう。ちなみにこの場合は、どの型の値がほしいかは、利用者が指定する必要がある。例はencoding/json。

```go
type Loader interface {
	Load(ob interface{}) error
}
```

genericsがあれば、もう少しマシになる。

```go
type Loader [T] interface {
	Load() []T
}
```

paginatorを利用した関数みたいなものを書く時に力を発揮する。

```go
func Iterate(loader Loader[T]) Paginator[T] {
	...
}
```

### goroutine

いい感じに複数のresourceからの入力を読み取る時。

- nil channel
- receiveしたときの第二引数

### interface乱用の読みづらさ

### loggerの悩ましさ？

### NewFromConfig

- 利点: configの構造を後で自由に変えられる
- 欠点: configの型と各モジュール側のstructとの変換を書く必要がでてくる(?)

### なぜTDDから遠くなるか？

リズムがつかめない。integration testになると時間がかかりすぎる。本当？

### 忘れがちなidiom?

- functional options
- errorgroup
- sync.Once
- https://github.com/tmrts/go-patterns/blob/master/profiling/timing.md
- return typed nil (nil interface value)

