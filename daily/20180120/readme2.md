#[golang][memo] goでのslack通知(mail通知)のコードについて考えたりしていた

goでのslack通知(mail通知)のコードについて考えたりしていた。とりあえずの段階ではdispatcherという構造で管理するのが良さそうという感じにはなった。
ただ、色々な階層でのテストなどのことを考えるともう少し考えることがありそうな感じがしている。


## dispatcher的なものの実装について考える

dispatcherの定義

> 主たる処理に影響を及ぼさない処理を呼び出す何か

例えばの例

> ある処理の終了後にslack通知を行う

ここでslack通知が仮に失敗したとしても、実際の処理の成否には影響を与えない。

ここでのdispatcherの実装方法について考えてみることにする。
(:warning: この文章中ではprivateになっているものは大抵の場合public(小文字である必要はあまりない))

## dispatcherが欲しくなる背景

### 直書きはありえない

例えば以下の様なすべての処理が直書きされているような関数は良くない。

```go
func runX(){
	doSomething()
	notifySlackForX()
	doSomething2()
}
```

実行の度に何らかの通知(ここではslack通知)が発生するし。直接設定値を埋め込んでしまっているなら実質実行を他の環境向け(本番以外のstagingやtest環境)に試すことができない。

### 設定値を受け取るように

それはさすがにダメなので。通常は通知先などの設定値を受け取るはずなのでオブジェクトにする。実行時にようやく設定値を見ると言うのは避けたいためconfを見てオブジェクトを生成する関数を用意することが多い。

```go
type slackNotifier struct {
	destination string // 通知先が複数の場合にはもう少し凝った構成にする
}

func newSlackNotifier(dest string) *slackNotifier {
	return &slackNotifier{destination: dest}
}

func (n *slackNotifier) notifyForX(status string) {
	fmt.Println("----------------------------------------")
	fmt.Printf("slack (%s)\n", n.destination)
	fmt.Println("...", status)
	fmt.Println("----------------------------------------")
}

func runX(n *slackNotifier) error {
	result, err := doSomething("X")
	if err != nil {
		return err
	}
	n.notifyForX(result)
	if err := doSomething2("X"); err != nil {
		return err
	}
	return nil
}
```


設定値を受け取れるようにしたので少なくとも通知先は変更できるようになった。アプリ自体の実用上はこれで問題ないし。最悪必要な状況毎に環境を作成してあげれば手動でのテストが可能になる。

```go
func main() {
	type config struct {
		Slack struct {
			Destination string `json:"destination"`
		} `json:"slack"`
	}

    // 実際のアプリでは何か設定ファイルに書かれている
	var c config
	s := `
{
  "slack": {
    "destination": "#dev-x-notif"
  }
}
`
	if err := json.Unmarshal([]byte(s), &c); err != nil {
		log.Fatal(err)
	}

	slackNotifier := newSlackNotifier(c.Slack.Destination)
	runX(slackNotifier)
}
```

### 通知方法は固定ではないかもしれない

通知方法は固定ではないかもしれない。何か変わるかもしれない。例えば、通知先がslackではなくdiscordに変わるかもしれないし。メールも飛ばすかもしれない。通知方法が変わる度にいろんな箇所の引数を変えてまわったり繋ぎ方を変えるというのはひどく馬鹿馬鹿しい。

```go
// 元々の定義
func runX(n *slackNotifier) error {
...
}

// discordに
func runX(n *discordNotifier) error {
...
}

// discordに、mailも使う
func runX(n *discordNotifier, n2 *mailNotifier) error {
...
}
```

この時通知に対する何かを行うオブジェクトみたいなものを考える。外から見たときにはどのような手段を使って通知されるかは意識する必要がなく、内からみればその時時で必要になった手段を使って通知が行われるようなもの。これをdispatcherと呼ぶことにする。

例えば、Xという処理に対する通知が存在するとして、それをdispatchForXとする。これを持ったstructを作る。

```go
type defaultDispatacher struct {
	slackNotifier *slackNotifier
}

func (d *defaultDispatacher) dispatchForX(status string) {
	d.slackNotifier.notifyForX(status)
}

func newDispatcher(dest string) *defaultdispatcher {
	return &defaultDispatacher{slackNotifier: newSlackNotifier(dest)}
}
```

そして実行時に受け取る依存はこのdispatcherだけにする。今度は通知方法が内部的に幾ら変わったとしても、それを使う関数の実装には変更が無い

```go
// 今までもこれからもずっとdispatcher
func runX(d *defaultdispatcher) error {
...
}
```


### 実際の処理には依存したくないかもしれない

テストで必要になる度に実際の環境を用意するということは無理に近い。特に通知のように何らかの外部サービス的なものへ影響を与えるようなものは特に。こういうものは普通interfaceにする。

先程のdispatcherの定義に違和感のある名前を付けていたことから分かる通り通常はここまでを基本にするような気がする。

```go
type dispatcher interface {
	dispatchForX(status string)
}
```

そして使う分にはinterfaceだけを気にする。

```go
func newDispatcher(dest string) dispatcher {
	return &defaultDispatacher{slackNotifier: newSlackNotifier(dest)}
}

// 今までもこれからもずっとdispatcher
func runX(d dispatcher) error {
...
}
```

dispatcherが誕生した。とは言え、これだけだとけっこう不都合があるのではというのがこのメモの主題。

## dispatcherの実装に求めるもの

dispatcherの実装に求めるものは以下の様な感じ

- dispatcherで行われる処理の条件についてテストを書きたい
- 他の処理のテストを書く時に邪魔にならないようになっていてほしい
- 実際のアプリの実行では設定不備などがない状態であってほしい
- (interfaceが肥大化していく問題)

### dispatcherで行われる処理の条件についてテストを書きたい

dispatcherで行われる処理の条件についてテストを書きたい。この種のちょうどテスト対象がmockで行われる部分に直接関係してくる部分のテストはinterfaceでwrapした段階でほぼ自動で付いてくる。mockを使ったテストをすれば良い。

interfaceでwrapしたstructのテストにいわゆるmock的なライブラリを使ってテストをする。
例えば、[stretchr/testify/mock](https://github.com/stretchr/testify#mock-package)などを使う([vektra/mockery](https://github.com/vektra/mockery)などを使って生成することもあるかもしれない)。

dispatch_test.go

```go
package main

import (
	"testing"

	"github.com/stretchr/testify/mock"
)

type mockDispatcher struct {
	mock.Mock
}

func (m *mockDispatcher) dispatchForX(status string) {
	_ = m.Called(status)
}

func TestRunX(t *testing.T) {
	d := &mockDispatcher{}
	d.On("dispatchForX", mock.Anything)
	runX(d)
	d.AssertNumberOfCalls(t, "dispatchForX", 1)
}
```

上記のようなテストコードはよくある形。interfaceを満たしたmockを利用する形に差し替え、呼ばれた時の引数や回数などをテストする。
(通常、条件により通知が行われる/行われないの分岐が存在するのでその分岐をテストするtable driven testみたいな形になる)

#### (通知内容のテストについて)

ところで通知の条件に対するテストの他に通知内容のテストもしたくなることがある。こういうことを考えると、notificator的なものはclient(communicator)とそれ以外に分離しておくと便利だったりするかもしれない。

例えば、slack通知でtemplateを利用して整形したメッセージを通知したい場合には、メッセージの構築部分のテストをしたくなることがある。ここで送信はしてほしくないのでcommunication部分に更に分ける。

### 他の処理のテストを書く時に邪魔にならないようになっていてほしい

他の処理のテストをするときにはどうだろう。例えばrunXがより枝葉的なコードだったとして、それを呼び出したりする部分のテストのときにはどうするか。例えば結合テスト的なものを書く時に、理想的な話をすればdispatcher用のmockが欲しくなるということを意識せずにテストを書きたい。このあたりでちょっとした仕組みが欲しくなったりする。

例えば以下のようにrunXを呼び出すappみたいなものがあるとする。実際のコードはこれより複雑でdispatcher以外にも色々依存するものがあるとする。

``` go
type app struct {
	dispatcher dispatcher
}

func (app *app) run(call bool) error {
	fmt.Println("*")
	if call {
		runX(app.dispatcher)
	}
	return nil
}
```

testify/mockの場合には呼ばれるものに対応するhookをテキトウにdefaultで設定しておくヘルパー関数があればまだまし。
特にgomockではなくtestify/mockを使っている場合には呼ばれることが期待されないmethodに対してのhookを設定してあげてもエラーにならないので便利。

```go
package main

import (
	"testing"

	"github.com/stretchr/testify/mock"
)

func TestApp(t *testing.T) {
	fakeDispatcher := func() dispatcher {
		md := &mockDispatcher{}
		md.On("dispatchForX", mock.Anything).Return(nil)
		return md
	}

	t.Run("通知が呼ばれる場合(本来のテスト対象は通知とは関係無い(通知のことは考えたくない))", func(t *testing.T) {
		app := &app{dispatcher: fakeDispatcher()}
		call := true
		app.run(call)
	})
	t.Run("通知が呼ばれない場合(本来のテスト対象は通知とは関係無い(通知のことは考えたくない))", func(t *testing.T) {
		app := &app{dispatcher: fakeDispatcher()}
		call := false
		app.run(call)
	})
}
```

例えばこのfakeDispatcherにあたるものがテスト用のutility packageにあるとまだましかもしれない(この記事ではprivateなものも実際のコードではpublic)。

## 実際のアプリの実行では設定不備などがない状態であってほしい

逆にテストではなく実際に動くアプリケーションの方のことを考えてみる。この場合にはinterfaceを利用するということはnilを許容することということに等しい。なので事前にdispatcherを必要とする部分でdispatcherが設定されていない場合はエラーになっていて欲しい。

以下のようにdispatcherを設定し忘れるということはないようにしたい。

``` go
func main(){
	// ...
	app := &app{}  // dispatcher忘れ
	call := true
	app.run(call)
}
```

処理の実行後に初めて設定を忘れていたことに気づくというのは良い状況ではない。今回の例で言えばappを作ったタイミングで設定の不備(ここではdispatcherがnilであること)を検知して欲しい。

```
*
  do something for X
panic: runtime error: invalid memory address or nil pointer dereference
[signal SIGSEGV: segmentation violation code=0x1 addr=0x20 pc=0x4b52e8]
```

直接structを利用してappを生成しないという方針は有効ではある。

```go
func makeApp(c config) *app {
	return &app{
		dispatcher: newDispatcher(c.Slack.Destination),
	}
}
```

呼び出しは以下の様になる。必ず設定ファイルからobjectを作る。ということを徹底する。

```go
	app := makeApp(c)
	call := true
	app.run(call)
```

### いろいろな用途のappがたくさんある場合

もちろん、makeAppに類する関数が１つで済めば問題は無いのだけれど。プロジェクトが大きくなっていくに連れてappの数が増えることになったりする(あるいはappの構造や依存が肥大化していく)。

このとき、makeAppに類する関数を複数回定義したりする場合が出てくる。この時忘れずdispatcherが利用可能なことをチェックしたい(本当は)。

## (interfaceが肥大化してく問題)

あと、コードサイズが大きくなるにつれて別の問題も出てくる。具体的にはdispatcherの定義が肥大化していく。

もちろん、mockのコードを自動生成するということにしてしまえば作業の手間という意味では困りはしないのだけれど。個人的にはinterfaceはt必要最小限で済ませるほうが好きではあるし。どことなく見通しが悪い感じになる。

```go
type dispatcher interface {
    // ふつうはもう少しまともな名前ではあるけれど

	dispatchForX1(status string)
	dispatchForX2(status string)
	dispatchForX3(status string)

	dispatchForY1(status string)
	dispatchForY2(status string)

	dispatchForA(status string)
	dispatchForB(status string)
	dispatchForC(status string)
}
```

このあたりは何でもかんでも共通constのpackageを作ると同じ感じで依存が膨れ上がる感じになるので良くないように思う。embedなどを利用して解決した気になるということはできそう。

```go
type ForXDispatcher interface {
	dispatchForX1(status string)
	dispatchForX2(status string)
	dispatchForX3(status string)
}

type ForYDispatcher interface {
	dispatchForY1(status string)
	dispatchForY2(status string)
}

type ForABCDispatcher interface {
	dispatchForA(status string)
	dispatchForB(status string)
	dispatchForC(status string)
}

type dispatcher interface {
    ForXDispatcher
    ForYDispatcher
    ForABCDispatcher
}
```

例えば、xというpackageにForXDispatcherというものを用意して、xパッケージの中ではdispather自体ではなくForXDispatcherに依存すると言ったコードにしておく。などとすればある程度はきれいにまとめられるように思う。

そしてappのconfから生成されるdispatcherが必要なdispatcherを満たしていない場合にはコンパイルエラーになるはず。

ところでこれでもnilチェックは解決しない。
