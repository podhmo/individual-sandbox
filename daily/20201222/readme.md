## go channel

- https://go101.org/article/channel-closing.html
- https://nanxiao.gitbooks.io/golang-101-hacks/content/posts/nil-channel-vs-closed-channel.html
- https://nanxiao.gitbooks.io/golang-101-hacks/content/posts/select-operation.html
- https://golang.org/ref/spec#Close
- https://stackoverflow.com/questions/8593645/is-it-ok-to-leave-a-channel-open

この図は便利

| Operation type | Nil channel | Closed channel |
| :--- | :--- | :--- |
| Send | Block | Panic |
| Receive | Block | Not block, return zero value of channel's type |
| Close | Panic | Panic |

close時の仕様自体は[ここ](https://golang.org/ref/spec#Close)に書いてある。

closeされたchannelをreceiveしたときに、multiple valueで受け取れば2つ目の値にcloseされたかの情報がやってくる。https://golang.org/ref/spec#Select_statements

```go
case i3, ok := (<-c3):  // same as: i3, ok := <-c3
	if ok {
		print("received ", i3, " from c3\n")
	} else {
		print("c3 is closed\n")
	}
```

defaultを使わずに複数のchannelをselectで待つ

- closeされたchannelはzero値とokを返し続ける
- nil channelは永遠にblockされる (selectの対象から外れる)

つまり、終わったらnilにしてあげないとだめ

```go
for ch1 != nil && ch2 != nil {
    select {
    case x, ok := <-ch1:
        if !ok {
            ch1 = nil
            break
        }
        ......
    case x, ok := <-ch2:
        if !ok {
            ch2 = nil
            break
        }
        ......
    }
}
```

- https://nanxiao.gitbooks.io/golang-101-hacks/content/posts/select-operation.html
- https://stackoverflow.com/questions/13666253/breaking-out-of-a-select-statement-when-all-channels-are-closed

## 複数のwatcherへのsend

channel closeが使える。for loopも行ける。

### いろいろ読んだけど

この記事だけでよいのでは？

- https://dave.cheney.net/2013/04/30/curious-channels
