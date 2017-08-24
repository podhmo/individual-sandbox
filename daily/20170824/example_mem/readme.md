## memory使用量調べる

- go本体のmemtestを見る

### go本体のmemtestを見る

`/opt/local/lib/go/src/runtime/pprof/mprof_test.go`


割り当てる処理

- transient1M
- transient2M
- persistent1K

ふつうに、packageのtoplevelに存在する変数にたいして代入したりを繰り返しているだけっぽい。

詳細

```go
var memSink interface{}

func allocateTransient1M() {
	for i := 0; i < 1024; i++ {
		memSink = &struct{ x [1024]byte }{}
	}
}

//go:noinline
func allocateTransient2M() {
	memSink = make([]byte, 2<<20)
}

type Obj32 struct {
	link *Obj32
	pad  [32 - unsafe.Sizeof(uintptr(0))]byte
}

var persistentMemSink *Obj32

func allocatePersistent1K() {
	for i := 0; i < 32; i++ {
		// Can't use slice because that will introduce implicit allocations.
		obj := &Obj32{link: persistentMemSink}
		persistentMemSink = obj
	}
}
```


samplingを止めて取得する場合にはProfileLateを変える

```go
oldRate := runtime.MemProfileRate
runtime.MemProfileRate = 1
defer func() {
    runtime.MemProfileRate = oldRate
}()
```

memoryのstatsを取るのは以下の様な感じ。

```go
runtime.GC() // materialize stats
var buf bytes.Buffer
if err := Lookup("heap").WriteTo(&buf, 1); err != nil {
    t.Fatalf("failed to write heap profile: %v", err)
}
```
