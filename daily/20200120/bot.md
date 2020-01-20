# botの種類

## architecture

内部で直接実行。CPU資源が問題。

```
bot[]
```

内部でworkerと通信。ちょっと処理が複雑になる。

```
bot[manager] <-> worker
```

botは通知をするだけ。管理するprocessの数は増える。zmqとか使うと楽（？）

```
bot[emitter] -> manager <-> worker
```

## 非同期対応

実際のところ非同期処理はそこまで必要ではない。この２つが別れていれば良いだけ。

- 処理を依頼
- 結果の取得

workerからbotへの通知の経路があれば大丈夫。

## 実装の方法

pythonの話

- botだけ
- botの中でmulti thread
- botがsubprocessを作ってstdin/stdoutのpipeで通信

  - (pipeのreadにselectorsモジュールを使う(kqueue,epoll的な))
  - (asyncioでsubprocessを作る)
  - (asyncioでsubprocessとの通信も作る)

- botがsubprocessを作ってzmqなどで通信

  - zmqのclient/serverを双方向に立てる

## 準備

- processの起動・死活監視 (e.g. subprocessのspawn)
- processの存在確認。ペアリング (e.g. subprocessを開いたときにsecret keyを)
- process間のメッセージング/コミュニケーション (e.g. zmq)
- 伝達されたメッセージの開封/封緘 (e.g. JSONRPC)

