# monogusa

どういう感じか気持ちは以前書いた気がする。[これか](../../20200122/example_monogusa/readme.md)

ちょっとだけmultiprocessingに心を惹かれたがqueueをmainの処理でしか使えないのでworkerを立ち上げていく方針では向いていなかった。あとsqsなどに差し替えた場合のことを考えると、素直にnamed pipeを使うのが良いような気がした。

## とりあえず良い感じのinterfaceを模索する

python 自分がほしいbackground taskを整理

- zero dependencyで試せる
- cloud上のサービスの利用に簡単に遷移できる (e.g. sqs)
- worker側の操作に特殊な依存を持ち込まない
- resumeができる
- managerとworkerをprocessごと分離できる
- workerを立ち上げっぱなしにしておける

### 適切な名前

- network部分, transport?

  - ReaderTransport, recv
  - WriterTransport, send

- marshal, unmarshal

  - JSONRPCMarsahaller
  - JSONRPCUnmarshaller

とりあえずそういう感じ？

## 追記

### 01

とりあえずnamed pipeでのecho server, echo client的なものを生成してみる。肝はRPC的なものではなくmessage box的なものということなんだろうか？

### 02

multi workerな感じにしてみる。これはまぁループしているだけ。

### 03

メッセージを行ごとに送る形は止めた。少なくとも別のworkerのものは混在して遅れるようにするということを考えると一番シンプルなのはこの形。jsonrpcなどのフォーマットに載ればそのままエラーなどはそのプロトコル上で生成してあげれば良さそう。

### 04

layerを分ける事を考えてみた。この辺りで名前をどうしようかということを悩みだした。websocketsのsend,recvだけの構造はかなりシンプルで良いような気がしている。この上に何らかのprocolを乗せるという感じのほうが良いんだろうか？そしてcontext managerを使うこともできるようにするとちょっとオブジェクトの定義が複雑になるなー。

あと、この辺りでサブプロセス前提ではないworkerの形式も欲しいなーと思い始めた。可能ならresumeに対応しているようなもの。queue.Queueはthreadingのprimitiveを含んでいるのでpicke化はできなかった。


### 05

tinyrpcのjsonrpcを使い始めた。ちょっとtinyrpcに依存するのもどうかと思い始めたのだけれど。どうしようかな。PRを出しまくるのと。。まぁとりあえずserialization/deserializationのprotocolとしてtinyrpcのものを使うか。

### 06

いきなりファサードから入りすぎたような気がする？websocketsみたいにsend()とreceive()ができるようなオブジェクトを作るところから始めるのが良いのでは？

知りたいのはオブジェクトの名前か。

- WebsocketClientProtocol
- WebsocketServerProtocol

まぁ妥当な話かも。

テキトーにそれっぽい感じの記述で動かせることを目指したい。

### 07

本当はやっぱりRPC的なインターフェイスが望まれているのかもしれない。
それはそれとしてprocess通信を模したようなコードがかけると便利。
named pipeを直接使うコードって考えてみると、準備をserve側にやらせても良いのではないか？

そしてpipeに渡すのがtransformer的なinterfaceになり、これは結局の所nodejsとかのstreamでは？

しかしjsonrpcのrequestオブジェクトに直接触るのは気持ち悪いな。

### 08, 09

とりあえずserve,connectで作成して、recv,sendで読み込むだけにしてみた。
ちょっといじってmsgpackrpcを使ってみたけれど。kwargsのサポートがないのだなー。

### 10

threading的なinterfaceはどうやって作れば良いんだろう？
(エラー処理の対応もしていなかった)

spawnしているだけで負け。と思ったけれど。関数を登録させれば良いのか。

### 11

adapterを書いてあげてthreadingでも動くようになった。
threadingである必要が何故あるかというと、readとwriteが同時に起こって固まる可能性があるから？

### 12

executorを使うことでサブコマンドとしても登録できるようにしてみた。
threading executorと簡単なコードは共有できる様になった。

### 13

threadingを使わずそのままsendを見たくない？
producer,consumerの形式ならgeneratorで十分では？

まぁそれはともかくFakeのproducerを作ってみる。考えてみるとidentity serializer的なものもほしい。

### 14

今度はfake consumer。consumerの中ではJSONRPCRequestが使われているのでちょっと困るかもしれない。
そしてSenderとReceiverをFakeで作る必要はないのではないか？

そしてgeneratorで書きたい。

sendとrecvを同時にしたくなった瞬間にgeneratorでは無理になる。
sleepあたりもfakeにしちゃいたいな。

### 16

minitask3を作った。fakeも取り込んだ。

### 17

一旦これまでを総括してもともとの期待する動作のカタチへの適用を考えてみる。


## 例外が発生したときどうする？

- 自動で処理する場合
- 人の手で処理をする場合

あとsubprocessのterminate, subprocessのsinal handing, stdin,stdout, 設定の継承

途中

- minitask3
- 例外の対応

