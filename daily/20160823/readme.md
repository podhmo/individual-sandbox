# golang もう一回pipelineのこと整理する

- [Go Concurrency Patterns: Pipelines and cancellation - The Go Blog](https://blog.golang.org/pipelines)

## これは個人的に勝手に付けた名前

- producer
- transformer
- consumer


```
// pn = Producer N, tn = Transformer N, Cn = Consumer N
p0 -> t0 -> t1 -> t2 -> ... -> c0
```

これと並列度の調整がある。

## fan-in,fan-out

- fan-in: `ProducerがN個 -> Consumerが1個`
- fan-out: `Producerが1個 -> ConsumerがN個`

fan-inするために `merge()` を作っている。内部では `sync.WaitGroup` で状態管理。

# 並行処理の無理矢理なwait

example_wait

- 00全部channelで
- 01sync.WaigGroup
- 02まじめにエラー管理するにはerrorを格納するmapにlock掛けないとだめ？
- 03goroutineの数を制限したい

普通にsync.WaigGroupの方が便利な感じはする。


