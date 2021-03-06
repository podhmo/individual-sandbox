# monogusa

- https://github.com/podhmo/monogusa

## やりたいことをまとめたい

### background task

やりたいことはbackground task。もう少し正確に言うならmanager側のprocessを負荷から守りたい。ここでのmanagerとは例えばslack botで言えばbot部分。実際の処理はworkerに投げてmanger process自身は負荷から自由になっていてほしい。

ここで以下２つの選択をする必要がある

- botはcommunicationのみを管理する。managerを別途立てる
- botはmanagerを兼ねる。ハンドリングが少し複雑になる。

後者で良い気がしている。

### 制約

ここで単に動くというよりは便利なものを作っておきたい。ここでほしいのはコスト的な感覚。精神的コスト。

- 手軽に動作を試したい
- 手軽に動作環境を移行させたい

後者は概ねcloudに移行したいということ。ということでコミュニケーションはSQSなどにencodeできるような仕組みが良い。もう少し加えるならmanagerからworkerへの通信に即時性を求めたくない(単純にRPCでつなげるということは避けたい(managerからworkerへの通信に即時性をもたせたくないという話と、managerが送信するrequestがすぐに返ってくるべきという話は別な点に注意。例えば、間にメッセージキューなどが存在した場合に、後者だけを満たすということは普通にありえるしそれを念頭に考えている))。

ここまでは明らか。次にどうやって通信を制御するかという話。双方向のチャネルをもたせるのが良いかと言うのは悩みどころ。わかっていることをまとめると以下。

- 親(manager)は子(worker)のendpointを把握している
- 子は何らかの非同期タスクを実行する。その非同期タスクの結果の通知は親の役割
- 最初の親の通信の応答がすぐに行われることは期待できない(?)
- 子がタスクを実行している最中は、親は暇になっていてほしい
- 親自身の操作がこのコミュニケーションによって妨げられることは合ってほしくない

非同期IOが必要かというとなんとも言えない。以下のようなbufferingが行われることで十分では？

```
1. 最初にworkerへtaskを依頼 (enqueue)
manager -> <endpoint>
             |
             |---> <Queue>
             |             
    <---------
結果は返さないが直ぐに反応は返ってくる

2. (どこかのタイミングで)workerはenqueueされたメッセージを取り出す。適当に実行する
              <Queue>
               \----------> worker Run()

3. (終わり次第）応答用のQueueに結果を返す
              <Queue2> <--- worker

4. (どこかのタイミングで)managerはenqueueされたメッセージを取り出す (dequeue)

manager <----- <Queue2>
```

つまり操作自体は非同期IOを利用する必要はない。必要なのはsubprocessによる実行だけ。

## 非同期IOの価値とは?

それでは非同期IOの価値とはというと、複数のIO待ち操作を束ねられる点。例えば何らかの通信を行うような操作はほとんど待ち時間。この待ち時間を束ねて実行できる。例えばbotの主たる操作が通信だけになるのだとしたら、workerなど作る必要もなくこれも兼ねる事ができる。

一方でそうはならないことが多いのでworkerを作りたいという話。あるいは何か特別コストの高い操作がある影響ですべての応答が遅延してしまうということが起きうる。これを避けたい。ここでも非同期IOを使っていれば待ち時間は無視できるがCPU時間がネック。

とはいえ実行時間自体が短くなりうるという点は魅力的で、worker側の実装にasyncioのloopを使うというのは良い話。というか多分ふつうにそうする。

## 通信の方法は

手軽さなどからpipeでコミュニケーションすることを考えていたが。これで扱うと面倒が起きるんだろうか？そうでもない気もする。ここを非同期IOにしておきたい理由はbot側の待受け部分も束ねたいからかもしれない。まぁsubprocessを直接で良い気もする。selectorsモジュールを使うのがthreadingを作るよりは良いかもしれない。

余裕があったらここもasyncioにしておきたいところではある。

## JSONRPCなどは？

勢い余ってJSONRPCなどをいじったりしてしまったが、これは何を念頭に入れてたのだろうか？どうつながってくるかというとserialization/deserializationのプロトコルをそっくりパクりたいという話。つまるところデータの転送のためのwrappingのためのフォーマットとデータ自体のフォーマットは別物（たとえばJSONRPCのプロトコルに乗せるということはエラー用の通信のことを考える必要がなくなる）。ただそれだけの話。

これが通信の経路を変えるという話になるとどうなるのだろう？SQSのbodyに全体を載せたいかどうかという話にもつながっていきそう。sender,receiverがオブジェクトとして存在していれば良いというだけなのではないか？

pub/subという名前にしたくない理由は？topic (subject) をなんとはなしに送信するという形ではないから？ある意味ここはそうしても良いような気はしている。

## SQSにencodeしたとき

わかっていなかったのは例えばSQSなどにencodeしたときに、保持するデータはどうなるべきかという話かもしれない。これはいくつか可能性はある。何らかのstoreにデータ自体は保存をしてそのアドレスだけを送る形式。あるいはデータ自体を送る形式。後々のことを考えると前者の方が良い気もするが、単にstreamを購読するだけといった場合には後者で良い。これはstreamがhotであるかcoldであるかという話に近い。基本的にhot streamというのは垂れ流しで不要なら捨てるという運用にするのが良い。

ここで気にしたいのはコードを書くときに扱うオブジェクトがどのような値を返すか？という話な気がしている。答えはアドレスではなくデータ自体。これは内部の通信自体の話、つまるところSQSで運用するならSQSのメッセージ自体にデータ全体を保持して送るという話ではなく、もしアドレスだけを送る通信ということにした場合には、そのクライアントオブジェクト自体がメッセージの購読だけではなくデータの取得もした結果を返してほしいということ。

つまりはここで仮にreceiver,senderというオブジェクトを定義したとしたら、そのオブジェクトのメソッドを利用するユーザー自身は展開されきったデータをハンドリングするという解釈で十分ということになる。それは正しそう。

## RPC的な操作は本当に不要か？

RPC的な操作は本当に不要なんだろうか？そもそも環境が閉じていてremote先の実態がpython processならmultiprocessingが手軽かもしれない。pickleで良い感じに通信してくれる。それこそ関数を指定しての実行ができる。

なぜRPC的な操作を行いたいかというと、一つのendpointで複数の操作を兼ねられるため。もしそうではない場合には必要な操作ごとにworkerを作る必要が出てくる。複数のタスクを一つのworkerで解釈できるということは、待ち時間を一つに束ねられる可能性が出てくる非同期IOと相性が良い。

（もちろん複数のタスクを同一のworkerで実行するなら、ある特定のタスクの安定性が他のタスクの実行の安定性にも影響を与えることになる）

任意のエンドポイントを呼ぶ、正確に言うなら特定のコマンドに対するエンドポイントが動的に生成されそれに対してリクエストがしたくなる、ということがあるなら、RPC的な操作が必要になるということになる気がする。これはまぁあってほしい気がする？

## 行いたい通信について

そもそも整理すると通信は以下２種類がある

- workerが提供する操作 (ほぼcommand execution?)
- managerが提供する操作 (ほぼnotify)

managerからみた場合は、worker内部でcommandが複雑なイベントの連鎖を抱えているかどうかは気にする必要がない。その一連の連鎖をもって一つのコマンドとして認識される。

workerから見た場合に、managerが提供する部分というのは概ね最後の出力先以上の意味がない。関数におけるreturn文のようなもの。あるいは結果をただただ印字するだけのprint文。まぁいわゆる継続（といってもそこまで複雑に考える必要はなさそう。単なる出力程度）。

RPCとして提供したいということは、行いたい選択肢の候補が動的に増えるという意味。ちょっとしたコーディネートはしたいと思うかもしれない。ところで通知の方法自体は限定したくはない。例えばworkerの終了後の操作としてslack通知ということを限定したくはない。

どのような通知を行うかと言うのはmanager側の操作に委ねられるのだろうか？これはそうな気がする。つまりworkerがcallbackを指定してmanagerにsendすることはなく、managerが自身のcallbackを指定してworkerに呼ぶだけ。関数の実行を模したものを考えるなら自然な形。実装の都合でworkerが存在するが実際には見えない状態になっていてほしい。

