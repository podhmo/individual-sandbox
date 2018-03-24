# goのchannelや並行処理などに対する今の所の理解

---
これは完全に個人用のメモ
---

channelが欲しくなるのは、並行処理中にデータの受け渡しがしたくなったとき。goの実行イメージとしてpromise的なものは存在せずすべて同期というイメージで書くと良い。一方で誰が動かすか的なものは変えることができ。それはgoroutineを動かすことによってできる。ついでにlwpっぽい感じになる。

ところですべてのgoroutineがdeadlock的な状態になったりして進まなくなったりするとpanicする。

あと、初めの内はchannelを使わなくても良いのではないかという気持ちがある。

## 最初の並行処理

以下の様な感じで無造作に呼び出したい場合は単にgoroutineを起動するだけ。

```go
go func(){
	time.Sleep(100 * time.Millisecond)
	fmt.Println("one")
}()
go func(){
	time.Sleep(100 * time.Millisecond)
	fmt.Println("two")
}()
```

## 終了まで待つ

ところで上の処理はそれぞれのgoroutineが終了することを保証しない。通常のコマンドは何らかの処理が終了するまで待って終了という形になる。この終了を判定する処理にchannelを頑張って使うこともできるけれど。待つ場合にはsync packageのWaitGroupを使う方が良い。channelだけで頑張ろうみたいな気持ちは沼にハマる。

WaitGroupの使いかたは概ね参照カウンタ的なものをイメージすれば良いし。addした分だけdoneするというイディオムになっている。

```go
var wg sync.WaitGroup
wg.Add(1)
go func() {
    time.Sleep(100 * time.Millisecond)
    fmt.Println("	one")
    wg.Done()
}()

wg.Add(1)
go func() {
    time.Sleep(100 * time.Millisecond)
    fmt.Println("	two")
    wg.Done()
}()

fmt.Println("wait...")
wg.Wait()
fmt.Println("done")
```

## ちょっとしたエラー制御

さて、waitGroupのWaitは内部のカウンタが0になるまで待つのだから、gorouineの実行が途中で中断されてしまいDoneなどが呼ばれなかった場合に、終了しないということが起こりうる。とは言え簡単なものやpanicされるものについては概ねgoがruntimeで判定してくれて良い感じにエラーを出してくれる。

例えば途中で終了してwg.Done()を呼ばないだったりとか。

```go
go func() {
    time.Sleep(100 * time.Millisecond)
    fmt.Println("	two..")
    return
    fmt.Println("	..two")
    wg.Done()
}()
```

```
fatal error: all goroutines are asleep - deadlock!
```

あるいは途中でpanicする場合には、どこかでpanicが起きた時点で全体が死ぬ。

```go
go func() {
    time.Sleep(100 * time.Millisecond)
    fmt.Println("	two..")
    panic("hmm")
    fmt.Println("	..two")
    wg.Done()
}()
```

```
panic: hmm
```

Done()を呼ぶのを忘れずにという意味ではdeferを使ってDoneをすることが多い。

```
go func() {
    defer wg.Done()
    time.Sleep(100 * time.Millisecond)
    fmt.Println("	two..")
    return
    fmt.Println("	..two")
}()
```

特にエラー処理との兼ね合いで途中でerrorをreturnしたときにDone()の呼び忘れなどを避けるために。

## goroutineで呼び出した先でエラーになることもある

先程の段階でエラー処理の受け渡しということになったが。当然goroutineで起動する処理にもエラーが発生しうるし。発生したエラーは回収したい。例えば１つでもエラーが発生したら終了みたいな動作を考えるとする。

疑似コード

```go
for _, runAsync := range tasks {
	// 実際には並行で動く
	if err := runAsync(); err != nil {
		return err
	}
}
```

こういうような全体に対するandを取るような操作は、errgroupを使ってしまったほうが手軽。go getでインストールしておく必要がある。

```go
go get -v golang.org/x/sync/errgroup
```

ところでerrgroupの使いかたはWaitGroupとちょっと変わっていて、add,done,waitではなくgo,waitだけになる。結構シンプルで手軽。

```go
ctx := context.Background()
g, ctx := errgroup.WithContext(ctx)

g.Go(fn0) // fn0's type is func() error
g.Go(fn1) // fn1's type is func() error

if err := g.Wait(); err != nil {
	fmt.Println("err!", err)
}
```

内部的にはやっぱりWaitGroupを使っているのだけれど。Goはgoroutineの起動のgoと重ねたイメージで作られた関数で。ここに渡したタイミングで内部のWaitGroupがAddされる。Doneも内部で良い感じにやってくれる。エラーが発生したら最初に発生したエラーがg.Waitのところで取得できる。

## 値の受け渡し

エラー値の受け渡しも実質値の受け渡しだったのだけれど。errgroupに任せてしまっていたので改めて。値の受け渡しは共有資源を更新し合う方法とchannelを使う方法がある。ここでもまだ手軽に終わらせるならchannelを使わなくて良いように思う。

共有資源を扱うならロックをかけないと。おかしな値を返す。アトミックな操作ではないので。

```go
var n int
var wg sync.WaitGroup
wg.Add(100)
for i := 0; i < 100; i++ {
    go func() {
        defer wg.Done()
        n++
    }()
}
wg.Wait()
fmt.Println(n) // 85
```

lockにはMutexとRWMutexの２つがあるけれど。特に最初のうちは気にせず全部Mutexで良い気がする。ちなみにMutexは再入可能なlockではないことに注意。

```go
var n int
var wg sync.WaitGroup
var m sync.Mutex

wg.Add(100)
for i := 0; i < 100; i++ {
    go func() {
        defer wg.Done()
        m.Lock()
        defer m.Unlock()
        n++
    }()
}
wg.Wait()
fmt.Println(n) // 100
```

今度は大丈夫。mutexの方も考え方は単純でLockされている間は共有資源(上のコードではn)に触っている最中ということだし。使い終わったらUnlockして開放してあげれば良い。

まじめにパフォーマンスを気にするならsync/atomicのパッケージを使ったりなどしても良いけれど。日常的に書くコードで細かなパフォーマンスに気を配るという必要はあんまりない。特に粒度の大きめの並行処理ならロックのコストはほとんど無視できる。

mapを使うときも同様(実はstructのfieldなど共有しないものに関してはlockをかけなくても大丈夫なものもあったりはするのだけれど。最初の内はgoroutineで触りうるものに関しては全てlockをかけておいた方が無難)。

### mapでlock/unlockがめんどうならSyncMap

lock/unlockが面倒でmapを使いたいということがしばしばある。そういう場合にはSyncMapを使うと良い。ただ内部的にはinterface{}で扱うので常にキャストが必要。(あとsizeが取れないのが地味に面倒などの話もある)

```go
var wg sync.WaitGroup
var c sync.Map

wg.Add(100)
for i := 0; i < 100; i++ {
    i := i
    go func() {
        c.Store(i, i*i)
        wg.Done()
    }()
}
wg.Wait()

v, ok := c.Load(50)
fmt.Println("50*50=", v.(int), ok)
```

(`i := i` という部分は goroutineで起動する関数にiを渡す形でも良い)

## エラー値の受け渡し再び

エラーの受け渡しについて再び考えてみる。今回は先程とは少し異なり。並行で動かした処理中の全てのエラーを取得する。途中でエラーが出ても実行を止めない。同期的に書くならこういうイメージ。

疑似コード

```go
var errs []error
for _, runAsync := range tasks {
	// 実際には並行で動く
	if err := runAsync(); err != nil {
		errs = append(errs, err)
	}
}
fmt.Println(errs)
```

今までの気持ちで考えるならlockを使う感じになる思う。以下のように。

```go
var wg sync.WaitGroup
var m sync.Mutex
var errs []error

wg.Add(100)
for i := 0; i < 100; i++ {
    i := i
    go func() {
        defer wg.Done()
        v := rand.Float64()
        if v > 0.95 {
            m.Lock()
            errs = append(errs, errors.Errorf("invalid %f (%d)", v, i))
            m.Unlock()
        }
    }()
}
wg.Wait()

for _, err := range errs {
    fmt.Println(err)
}
```

ところでエラーを発生したそばから使いたいという場合にはどうすれば良いだろう？このように考えるとwaitGroupで全体を止めるという操作が良くないという形に変わってくる。
