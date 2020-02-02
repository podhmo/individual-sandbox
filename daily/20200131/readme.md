## python queueを良い感じに扱ってみる

LIFOQueue,Queue,PriorityQueueをいじってみるとこれの共通部分を取り出すのに嬉しさがないかもしれない。
それぞれを良い感じに自分で使えば良いような気がしている。共通部分をとろうとしたが上の痛みの方が多いような気がしている。

それはともかくgo-cloudのinterfaceもみておくと良いかもなー。

## go-cloud pub/sub

https://github.com/google/go-cloud/tree/master/pubsub

どういうバリエーションが有るのかというと

- awssnssqs
- azuresb
- gcppubsub
- kafkapubsub
- mempubsub
- natspubsub
- rabbitpubsub

そしてdriverにどういうstructがあるかというと

- Message
- Topic
- Subscription

### publish

https://gocloud.dev/howto/pubsub/publish/

```go
import (
    "context"

    "gocloud.dev/pubsub"
    _ "gocloud.dev/pubsub/<driver>"
)
...
ctx := context.Background()
topic, err := pubsub.OpenTopic(ctx, "<driver-url>")
if err != nil {
    return fmt.Errorf("could not open topic: %v", err)
}
defer topic.Shutdown(ctx)
// topic is a *pubsub.Topic; see usage below
...
```

内部での利用はこう

```go
err := topic.Send(ctx, &pubsub.Message{
	Body: []byte("Hello, World!\n"),
	// Metadata is optional and can be nil.
	Metadata: map[string]string{
		// These are examples of metadata.
		// There is nothing special about the key names.
		"language":   "en",
		"importance": "high",
	},
})
if err != nil {
	return err
}
```

- Metadata付きでSend

やっぱりそういう形か。

### subscribe

https://gocloud.dev/howto/pubsub/subscribe/

```go
import (
    "context"

    "gocloud.dev/pubsub"
    _ "gocloud.dev/pubsub/<driver>"
)
...
ctx := context.Background()
subs, err := pubsub.OpenSubscription(ctx, "<driver-url>")
if err != nil {
    return fmt.Errorf("could not open topic subscription: %v", err)
}
defer subs.Shutdown(ctx)
// subs is a *pubsub.Subscription; see usage below
...
```

内部での利用自体はこんな感じ。

```go
// Loop on received messages.
for {
	msg, err := subscription.Receive(ctx)
	if err != nil {
		// Errors from Receive indicate that Receive will no longer succeed.
		log.Printf("Receiving message: %v", err)
		break
	}
	// Do work based on the message, for example:
	fmt.Printf("Got message: %q\n", msg.Body)
	// Messages must always be acknowledged with Ack.
	msg.Ack()
}
```

- Receive()
- Ack()

## python mypy genericsの理解を整理

- https://docs.python.org/ja/3/library/typing.html#typing.TypeVar
- https://www.python.org/dev/peps/pep-0484/#covariance-and-contravariance
- https://mypy.readthedocs.io/en/stable/generics.html#variance-of-generic-types

型変数の指定

- `covariant=True` 共変
- `contravariant=True` 反変
- invariant 不変
- (bivariant) 双変

基本的にこれらの話はvarianceの話。sub typingをどう扱うかという話。通常はinvariant。

covariantのわかり易い例はcollection。
`A`とそのサブタイプ `B < A` が在ったときに、あるコレクション`F`があったとして`F[A]`と`F[B]`をどう扱うか？
広い型から狭い型への変換。`F[A]`が`F[A], F[B]`を許すということなので`{A,B}`から`{A}`に変換しているというわけでcovariant。

一方でcontravariantはその逆。`G`が在ったときに`G[B]`が`G[A], G[B]`を許す。

この辺は実はwikipediaが一番わかりやすかった。

### 具体例

covariant

- Awaitable
- Itarator, Iterable
- Collection
- Union

contravariant

- Callable

bivariant

- Generator, AsyncGenerator

invariant

- List
- Dict
- (Mutable container is invariant)

### protocol

Protocolも型変数を取れる。

- https://mypy.readthedocs.io/en/stable/generics.html#generic-protocols

あるgeneric Protocolがメソッドの戻り値として保持している型変数の型の値を返す場合には、covariantでなければいけない。(これはたぶんMutableなcontainerがinvariantであるという話と関連がある)

