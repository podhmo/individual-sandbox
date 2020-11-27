## inflexible

やりたいことをまとめる。
名前は inflexible が良いかもしれない。
いろいろ諦めた結果、手軽な物が手に入ると言う感じ。

### CLIとweb API

やっぱり、 CLI と web API は両輪なような気がする。片方が定義できたらそのままもう一方も定義されて欲しい。
例えば、以下のような操作が許されるとしたら、

```
POST /add {"x": 10, "y": 20}
POST /mul {"x": 10, "y": 20}
```

以下のような形で実行可能になっていて欲しい。

```
cli add --x 10 --y 20
cli mul --x 10 --y 20
```

### いろいろ妥協する

全部を自前で書くのは後回しで良いのではないか？

- validationは `go-playground/validator` などに任せちゃう

  - きれいな jsonschema や openAPI doc は諦める
  - e.g. required な文字列といえば、 空文字以外の文字列のこと

- はじめから高速な生成は諦める

  - いろいろ妥協しちゃう。後で高速化する。

- 高速な実行は諦める

  - 手軽に利用ができれば良い。ある程度のボトルネックは許容する。

- 他の言語からの手軽な利用は諦める

  - service provider は go 限定
  - もしかしたら、 client はいろいろな言語から使えるようにするかもしれない

    - とはいえ、最初は OpenAPI doc からの生成で良いのでは？


ドメインモデルレベルでのテストが実行できたら、アプリケーションの実行はすぐにできることだけは担保しておきたい。
公開が手軽。速度は気にしない。目指すのはgrpc付近。なぜgrpcではないのか？

### Input/Output と interactor

とりあえず関数を登録したらそれでおしまいにしたい。 interactor と呼ばれる関数を action として登録する。
interactor の引数は POGO (plain-old-go-object) だけ。この POGO を shape と呼ぶ（？）。

コンセプトは、内部の定義をそのまま利用すると言うところかもしれない。

### RPC

- JSONRPC を諦める -> OpenAPI docによる記述が手に入る
- きれいな REST API を諦める -> operationID の記述だけで済むようになる
- きれいな openAPI doc や jsonschema を諦める -> 既存の go の vaildation がそのまま使える

考えてみれば GraphQL が全部POSTなので、全部POSTでも良いような気がする。
そして全てで `content-Type: application/JSON` を期待しちゃえば良い。

- POST 以外使わない path はフラット

  - GET, HEAD, DELETE, PUT などは使わない
  - pathも存在せず、operationIDのみ
  - (optionなどはqueryとして扱う？CLIでは環境変数？)

### RPCのアクションの定義

interactor 的なものを渡して、RPC を定義しちゃいたい。
小さいうちは、本物の処理を行う関数自体を直接渡せちゃって良いのではないか？

```
func Hello(ctx context.Context, ...) (..., error) {
	...
}

router.Add(Hello)
router.Add(Hello, WithName("hello"))
```

### 関数中の引数の扱いは？

これはいろいろ考えたけど、以下で大丈夫だと思う。関数中の引数は以下のように扱われる。

- components とは go の関数もしくはインターフェイスのこと
- data とはいわゆる 値 のこと
- unrequired なものを許容する場合は、ポインターを使う

例えば以下のような型があるとする。

```go
type DBSession struct { ... }
```

コレを利用したアクション (RPC としてのメソッドとして公開するもの) として `Register()` を公開したいとする。そしてこの `Register` が DBSession に依存したいときにはどうするか？。通常の関数として利用するなら、こういう感じになる。

```go
// 登録されるユーザー
type User struct {
	...
}

func register(ctx context.Context, session *DBSession, user User, verbose *bool) (MSG, error) {
	...
}
```

ここで、 `*DBSession` がAPIから渡されるデータはなく、内部で定義した component として利用できて欲しい。

その場合には以下のどちらかを使う。

```
// interfaceを定義
type Session interface {
	...
}

// 関数として利用
type SessionFactory = func () *DBSession
```

例えば、後者の場合は以下の様にして使う。

```go
func Register(ctx context.Context, sessionFactory func() *DBSession, user User, verbose *bool) (MSG, error) {
	...
}
```

こうしておけば、これをhandlerとしてそのまま登録可能にできる。


### JSON中のobject,arrayなどはどうやって渡す？

とりあえず、全部ファイル名を指定して渡すで良いのではないか？ `@<filename>` をしてしたら、JSONファイルと仮定して読み込む。あるいは、 `file://<filename>` と言う形式のほうが汎用性は高いかもしれない。

いろいろ考えたけれど、実際的な対応は以下の様になっているのかもしれない。

- web API : `value` -> `data`
- cli : `primitive` -> `value` -> `data`

ここで `primitive` とは `string,boolean,int,float` のこと。`value` にはいわゆる `map[string]string` や `[]int` なども入る。そして `data` には、go側で定義した各種 `struct {...}` が入る。

そして `value` を web API というか JSON はそのままリテラルとして記述できるという感じ。
`primitive` から `value` への変換はファイル名を使うというような形。

JSON 表現から個々の型への変換は、完全に `json.Marshal()` や `json.Unmarshal()` に依存してしまう。そうしてしまえば何も気にする必要がなくなる。

こう考えてみると、 CLI のフラグ解析系のライブラリは `primitive` な値だけを管理できれば十分ということになる。validation などは web APIと同様に扱えば良い。はじめのうちは CLI でも全部一律で JSON 化する。

### code generation phase DI

↑のコンポーネントの解決をどこでするかといえば、コード生成をするタイミング。例えば CLI のコードを生成したり web API のコードを生成したりするタイミング。たとえば `registry` や `runtime` というようなパッケージを作ってしまう。今まで説明してきた `primitive` な値から、 components を生成するような処理を自分で書いてあげる。 例えば providers などと呼ぶと良いかもしれない。

基本的には、この DI は以下の手順で調べる

- 名前 + 型 で探索
- 型 で探索

通常は、型だけを気にしていれば良いのだけれど。例えば、 `time.Time` として 今日を表す `today` と今を表す `now` を使いたい場合がある。

### error response はそのまま application error を内部で使うことで解消する

interactor が返すのは error 値。例えば以下のようなインターフェイスを期待して、 `StatusCode() int` などを持っていたら、そちらを使うというような感じにすれば良いような気がしている。

```go
type ApplicationError interface {
	error
	StatusCode() int
}
```

なのでこまごまとしたハンドリングは存在しない。

### CLI と web API の両対応

CLI と web API の両方に対応するハンドラーの書き方はどうすれば良いんだろう？例えば、以下のような struct として扱う。

```go
type Event struct {
	Name string
    Headers map[string]string
	Body io.Reader
}
```

Body を持つべきか、 `map[string]interface{}` というような型の Params をフィールドとして持つべきかは悩みどころ。ただ一つ言えるのは、認証用の情報をパラメーターには依存しない形で持たせて置けると便利と言う気持ち。あとは `net/http.Request` に陽に依存するような形は避けたい。

- Adapter が入力を Event に変換する
- Router は Handler を選択する
- Handler は Event を受け取る
- Handler は内部で Interactor 用の値に変換して、 Interactor に渡す

例えば、今までの例をなぞらえると以下のような感じで動くイメージ？

```
func WebAPIAdapter(handler func(context.Context, Event) error) http.HandlerFunc {
	return func (w http.ResponseWriter, r *http.Request) {
		ev, err := Convert(r.Body) // TODO: drain?
	    if err != nil {
			SendError(r, err, 400)
			return
	    }
		defer r.Body.Close()

		ctx := req.Context()
		ctx = ctx.WithValue(key, ev)
		if err := handler(ctx, ev); err != nil {
			statusCode := 500
			if x, ok := err.(interface { StatusCode() int }); ok {
				statusCode = x.StatusCode()
            }
		    // logはどうしよう
			SendError(r, err, statusCode)
			return
		}
	}
}

func RegisterHandler(ctx context.Context, event Event) error {
	var input struct {
    	User
		verbose *bool `json:"verbose"`
    }
	if err := json.UnmarshalJSON(ev.Body, &input); err != nil {
		return err
	}

	sessionFactory := registry.SessionFactory
	return Register(ctx, sessionFactory, input.User, input.verbose)
}
```

handler は勝手に生成される。handler は CLI でも web API でも共有して使われる。例えば、CLIは以下のような形。

```go
func CLIAdapter(handler func(context.Context, Event) error) cli.HandlerFunc {
	return func (ctx context.Context, r io.Reader) error {
		ev, err := Convert(r)
	    if err != nil {
			return err
	    }
		ctx = ctx.WithValue(key, ev)
		if err := handler(ctx, ev); err != nil {
			return err
		}
		return nil
	}
}
```

#### registry

router と registry が必要になってくる？ どうだろう？
router 自体は、自前で定義しないで単にライブラリに渡される値として消費されそう (e.g. CLI なら flag パッケージなど。 web API なら例えば go-chi/chi のような router 用のパッケージに)

registry は どこかで初期化が必要。 config オブジェクトからいい感じに取ってくるような操作が必要になりそう。

#### interceptor

interceptorのようなものを追加したい。コレは多分必須。例えばリクエスト時のログだったりそういうものをすべてのアクションの実行前にかけたい。

### Grouped()

可能なら Grouped 的な操作もしたい。

### 認証・認可

認証は web API だけにかけたい。 CLI からのアクセスのときには一切かけたくない。
あと、認証は生成される側に組み込みたい。例えば、認証部分を別のサーバーで対応するというような操作を加えた時に対応できるようにしておきたい。

```
auth server -- Authoriazation header を見て Authorization header* を付け替え JWT (cache)
api server -- Authorization header* をただ見るだけ
```

### misc

:warning: コレは使わないかもしれない。

shape と model。 こういうのはどうなんだろう？

shape

```go
type User struct {
	Name string `json:"name"`
}
```

model の 集約 root などもこういう感じ。tree の root は、アクセス用の口を持っている。

```go
type User struct {
	Object *shape.User
	*database.DBSession
}

func FindUser(session *database.DBSession) (*User, error) {
	return &Users{
		Object: user,
		DBSession: session,
    }
}

func (u *User) FindTeam() (*shape.Team, error) {
	if team, ok := u.DBSession.Map["team:"+u.Object.TeamID]; ok {
		if team == nil {
			return nil, ErrNotFound("team")
		}
		return team
	}

	// (Unit of Work) や Identity Map みたいな機能がついていても
    team, err := u.FindByID("teams", u.Object.TeamID)
    if err != nil {
		return err
    }
    u.DBSession.Map["team:"+u.Object.TeamID] = team
	return team
}

type Users struct {
	Objects []*shape.User
	*database.DBSession
}

func FindUsers(session *database.DBSession) (*Users, error) {
	return &Users{
		Objects: users,
		DBSession: session,
    }
}
```

下のレイヤーではあんまり必要なものを考える必要がなくなる。

```
u := FindUsers(session)
team, err := u.FindTeam()
```

### misc

PK の発行は個別の機能として持たせたいよなー。

## hmm

- https://nrslib.com/bottomup-ddd-2/
