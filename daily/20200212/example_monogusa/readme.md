# monogusa

反応するbotを作りたい

[前回の調査](../../20200211/example_monogusa/readme.md)の続き

> 反応を定義したい

## 本当に反応が必要？

chatbot, web api, console appの中で、機能的にchatbotにより過ぎな気もする？
...そんなことはなくって、どうしても必要な操作な気がする。

というのも、コマンドとしての提供の場合には発動のタイミングを何かに依存させる事ができない。

- 特定の時刻に実行すること (cron)
- 定期的に実行すること (cron)
- 手動で実行すること

のどれかになる。そうではなくて何らかの操作に対して反応をさせたい。

反応にも実は２種類ある

- ある環境上でのあるユーザーの操作に依存した反応
- ある環境上で発生するイベントに依存した反応

前者はうまく作れば後者と同じものになりうる（？）が、今回欲しいのは前者の意味。
例えば、会話中の特定の表現を抜き出して何らかの操作を自動で行わせるという操作がやりたい (e.g. privateな環境のURLからbodyを抜き出して一部を引用。amazonなどへのURLを発見したら気になる的なbacklogに追加)。

後者はpush型のmessaging APIのようなものをイメージしている。これは見える場所に公開する必要があるし、リクエスト元の情報を元にフィルタリングする必要があったりする。今回はこれは対象外。

## やること

- それぞれのbot libraryのfakeを書いてみる
- それぞれのbot libraryでできることを調査してみる(事前調査)
- 良い感じのMessageについて考える

## 事前調査

実行のhookとしてどういう物が欲しい?

- message
- reaction

この２つそう。それぞれできるか調べる。

|| message | reaction |
| :--- | :--- | :--- |
| slackbot | OK | NG |
| discord.py | OK | OK|

なるほど。

まぁreactionは今回の主題ではないので良いや。

## 良い感じのMessageについて考える

既存のcliなどとも調和するような形で考えたい。

そもそも何を考えなくちゃいけないのだっけ？

- monogusaのCLI,WEBとも調和させること
- 複数のライブラリのmessageをまとめること

### 調和

あんまりやりたくないのはcomponentとして扱うこと。もちろん特別なcomponentということで引数にこれを渡したらというのはできるがちょっと避けたい気持ちがある。DIに機能を押し込むのはちょっと勘弁してもらいたい。

```python
def on_message(message: Message)->None:
    ...
```

とはいえ、何らかのwrappingというふうに考えると、完全に内部の情報だけを扱って考えるというのは無理。
例えば、WAFでいうRequestオブジェクトのようなものが必要になってくるのかもしれない。

これをどうやるかが課題。

#### CLI

ちなみにCLIのインターフェイスに沿う必要はある。これにどうやって対応する？

```console
$ <cli> <action> ...
```

コマンドの公開という範疇の場合には↑の`<action>`部が単に関数と対応するだけで良かった。
(このことにより、モジュール内の操作である関数を外の環境にアクセスするCLIに、ローカル上での操作を他の領域に公開するweb apiやchatbotのコマンドにと言う形で公開範囲を拡げられた)

ところで、今回は`action`に類似するものはないのではないか？あえて言えばreceive的な関数になる。こういう形で良い？

```console
$ <cli> fake-receive hello
```

とすると、cli.pyなどでの定義はどういう感じになるだろう？あんまりこういう形で書きたくはないが。。

```python
def receive(register):
    register("https://.*amazon.*", on_amazon_url_included)
    register("https://gist.github.com/([^/]+)/", on_gist_url_included)
```

素直にdiscord.pyのように`on_message()`みたいな関数で良いのではないか？
(これは後に追加されるかもしれない。startup,shutdown的なeventとも相性が良い気がする)

```
from monogusa.event import on_message

@on_message
def on_message(msg: MessageEvent) -> None:
    ...
```

さて、こうなったときにCLIのサブコマンドとしてどういう形にものが提供されるべきか。
(デフォルトで常に表示されていても良いかもしれない。defaultはpeekとかで)

```console
$ <cli> fake-send --event-type message hello
```

stdinの読み込みを使っても良いが。良いかもしれない。。

```console
$ cat messages | <cli> read # defaultは --input=text
$ cat messages.json | <cli> read -i json
$ cat messages | <cli> read -i text
```

### 複数のライブラリのmessageをまとめること

ここに関しては真面目に綺麗なフォーマットを考えるのはバカバカしい。そういうふうに考えるのは辞めよう。
とりあえずAdapterのような形で記述するのが良いと思っている。てきとうに必要なものだけを記述しておくと良い気がする。protocolで。

```python
class Message(t.Generic[T]):
    id: str
    content: str
    raw: T  # 生の表現
```

必要な操作は何があるだろう？つまるところcallbackは何かという話。

- 同じchannelにメッセージを書き込む
- 特定のchannelにメッセージを書き込む
- reactionを返す

最終的にはこのmessageはencodeされて別のprocessに渡される事も考えたい。なのでidentity的なものも保持している必要がある。

- どのメッセージから始まったか

  - id
  - channel
  - author (sender)
  - content (body)

```python
MessageType = tx.Literal["unknown", "message", "reaction"]

class Message:
    raw: t.Any

    id: str
    message_type: MessageType
    content: str
    author: User
    channel: Channel

    def reply(self: text: str) -> None:
        ...

    def send(self: text: str, *, chanel: t.Optional[Channel]) -> None:
        ...

class User:
    id: str
    name: str # display_name?

class Channel:
    id: str
    name: str # display_name?
```

そもそもこういうのは直和型で考えたい (tagged union)。event idとmessage idは別？urlはあればあると良いけれど。

### 再考

まぁ今回で言えばMessageEventだけがほしいのだけれど。

```python
Event = MessageEvent | ReactionEvent | UnknownEvent

class MessageEvent:
    raw: t.Any

    id: str
    content: str

    author: User
    channel: Channel

    def reply(self, text: str) -> None:
        ...

    def send(self, text: str, *, chanel: t.Optional[Channel]) -> None:
        ...

    def react(self, emoji:str, *, diff:int) -> None:
        ...

class ReactionEvent:
    raw: t.Any

    id: str
    message_id: str # message id?
    content: str # emoji
    diff: int # added=+1, removed=-1

    # need?
    author: User
    channel: Channel

class UnknownEvent:
    raw: t.Any

    id: str

class User:
    id: str
    name: str # display_name?

class Channel:
    id: str
    name: str # display_name?
```
