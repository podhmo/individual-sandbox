## go dispatcher的なもの

dispatcher的なものの実装について考える

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
- 他の処理のユニットテストを書く時に邪魔にならないようになっていてほしい
- 実際のアプリの実行では設定不備などがない状態であってほしい

