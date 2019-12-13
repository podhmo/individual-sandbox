## go goroutine with panic

以下を達成したい

- panicの発生した位置をlogなどとして出力したい
- panicをerrorに置き換えたい
- (コレを良い感じの例として表現したい)

最後のものは以下のようなMakefileを書いてあげると良さそうだった

### 良い感じの例として表現したい


```make
run:
	go run ${TARGET}*/main.go | tee $$(echo ${TARGET}*/)output.txt
	@echo "----------------------------------------"
	@cat -n ${TARGET}*/main.go | head -n $$(( $$(cut -d : -f 1 $$(echo ${TARGET}*/)output.txt) + 1 )) | tail -n 3

.DEFAULT_GOAL := 00

00:
	$(MAKE) run TARGET=$@
```

ちなみにこれは、以下のような形式で出力されることに依存している。

```
<lineno>:<filename>
```

e.g.

```console
$ cd ./example_go
$ make -s 03
make run TARGET=03
go run 03*/main.go | tee $(echo 03*/)output.txt
49:VENV/individual-sandbox/daily/20191213/example_go/03panic/main.go
----------------------------------------
    48		var p *person
    49		fmt.Println(p.Name) // nil panic
    50	}
```

### panicの発生した位置をlogなどとして出力したい

stack traceの取得自体は以下の２つがある。

- [runtime/debug.Stack](https://golang.org/pkg/runtime/debug/#Stack)
- [runtime.Stack](https://golang.org/pkg/runtime/#Stack)

手軽にやる分にはdebug.Stack()の方が便利。
一方でフルのstackを取りたい場合には、runtime.Stack()が使える。

ただいろいろ考えてみたけれど、panic値のgoroutineは、必ずrecoverするものと同じものでなければならない（なるはず）なので、debug.Stack()で十分かもしれない。

panicが発生した位置などはこんな感じで出力できる。

```go
pc, filename, lineno, _ := runtime.Caller(4)
fmt.Printf("%d:%s -- %s\n", lineno, filename, runtime.FuncForPC(pc))
```

runtime.Caller()の部分の数値は基本的には固定の定数で良さそうかも。
(もちろん組み込み方によって調整が必要で4ではないかもしれない)

### panicをerrorに置き換えたい

いろいろ考えたけど、errorのpointerをもらって、named returnsをするのが無難そう。

```go
// Recoverer ...
func Recoverer(p *error) func() {
	return func() {
		r := recover()
		if r == nil {
			return
		}

		switch r := r.(type) {
		case error:
			*p = r.(error)
		default:
			*p = fmt.Errorf("%v", r)
		}
}
```

利用する側はこう。

```go
func run() (err error) {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		recoverer := findcaller2.Recoverer(&err)
		defer recoverer()
		defer wg.Done()
		foo()
	}()
	wg.Wait()
	return
}
```

(厳密に言えば、errgroupのGo()のようなもので作られた関数の中でのnamed returnは `return nil` とも書ける。deferの使われているscopeと同じscopeなら `return nil` でも大丈夫)

```go
func run() error {
	g, _ := errgroup.WithContext(context.Background())
	g.Go(func() (err error) {
		recoverer := findcaller2.Recoverer(&err)
		defer recoverer()
		foo()
		return
	})
	return g.Wait()
}
```

例えばコレは `return` ではなく `return nil` でも動く。
