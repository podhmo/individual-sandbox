## python asyncio周りのmypyのことを知らないかも？

## python aioknife background?

backgroundの意味がようやく分かった。iterator的に使いたいやつだ。

## python asyncio strictな実装が欲しいかも？

loopがないと怒られるやつ。

## python asyncio subprocessとまじめに交流したい

## python asyncio そろそろ他のライブラリもまじめに見たい

aiohttpとか

- https://blog.hirokiky.org/entry/2018/11/23/102343

## python asyncio

- crawler?

1. 非同期request
2. 個数が不定のtask
3. 同時接続数制限
4. stop/retry
5. process数を増やして並列処理(queueの外出し)


## python asyncio

便利なasyncioってなんなんだろうな？

- debugがしやすい

  - 状況が把握しやすい
  - 途中の段階で止められる
  - 不要なステップを除外できる
  - 直列と並列を切り替えられる

- 書きやすい

  - 同期・非同期を手軽に変更できる
  - テストしやすい
  - 新しい知識が不要
  - 間違ったことをしたら教えてくれる

- 状況が把握しやすい

  - summaryが表示される
  - 至る所でhookが存在する
  - task間の依存がtraceできる？

hmm

既存のloggingをstructuredな表示に変えたい(magical)
summaryが表示される
hookを差し込みやすい
deployがしやすい

### ちょっと作業してみて

logはあるとメチャクチャ便利。
logを作ったらsummaryが欲しい。

ping的なタスクを追加したくなることがある。

- 自分自身のloop

priorityを指定したい

- queueを差し替え可能に？

gracefull shutdown

自身もawaitableになりたい(loopの外側からsub taskに)。
全体の並行数制限みたいなものも必要？(resourceという意味では？)


## python sigintでpdb

```python
    import signal

    def h(signum, tb):
        import pdb
        pdb.set_trace()

    signal.signal(signal.SIGINT, h)
```

## python asyncio Queueを使って不定回のタスク

aioknifeとかでわりと複雑なものを作ってしまっていた。
workerを定義して回すという方法なら手軽ではあるけれど。。。

### そもそもqueueの使いどころって？

- 単に値を入れておくだけだと寂しい
- taskを管理する方法(task_done()を使わないと)

一番手軽な方法は

- worker taskをN個立ち上げる
- waitで待つ
- cancel

昔の自分が書いた記事優秀だな。https://pod.hatenablog.com/entry/2018/09/22/215030

## python asyncio 世代交代

以下の関数/メソッドを使っていった方が良いかも？

- asyncio.run (get_event_loop() + run_until_complete()の代わりに)
- asyncio.Loop.create_task (ensure_futureの代わりに)

