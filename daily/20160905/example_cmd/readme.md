# golang cybozu-go/cmd見てみる。

## decorator pattern っぽい感じになっている？

e.g. cmd.HTTPServer は net.http.Serverをwrap?
(cmd.Serverと勘違いしていた)

https://github.com/cybozu-go/cmd/blob/master/example_test.go#L41

```go
	accessLog := log.NewLogger()
	accessLog.SetFormatter(log.JSONFormat{})
	accessLog.SetOutput(w)

	// HTTP server.
	serv := &cmd.HTTPServer{
		Server: &http.Server{
			Handler: http.FileServer(http.Dir("/path/to/directory")),
		},
		AccessLog: accessLog,
	}

	err = serv.ListenAndServe()
	if err != nil {
		log.ErrorExit(err)
	}
	err = cmd.Wait()
```

ちょっと必要そうな者のコードの整理

```go
func (s *HTTPServer) ListenAndServe() error {
	addr := s.Server.Addr
	if addr == "" {
		addr = ":http"
	}
	ln, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	return s.Serve(ln)
}

func Wait() error {
	return defaultEnv.Wait()
}

func (e *Environment) Wait() error {
	<-e.stopCh
	if log.Enabled(log.LvDebug) {
		log.Debug("cmd: waiting for all goroutines to complete", nil)
	}
	e.wg.Wait()
	e.cancel() // in case no one calls Cancel

	e.mu.Lock()
	defer e.mu.Unlock()

	return e.err
}
```

## １個見ていったけれどよく分からないので全体を見ることにした。

- :done: default.go
- :done: env.go
- exec.go
- graceful.go
- http.go
- idgen.go
- jsonlog.go
- log.go
- logfile.go
- logfile_windows.go
- reqid.go
- server.go
- :done: signal.go
- stopsig.go
- stopsig_windows.go


### environment.go 全体の設計

- Environmentというものが存在
- Environmentが内部にContextを持つ。

内部的にもう少し情報を持っていて、アクセス制御のためのlockだとか状態だとかを持っている

```go
// Environment implements context-based goroutine management.
type Environment struct {
	ctx       context.Context
	cancel    context.CancelFunc
	wg        sync.WaitGroup
	generator *IDGenerator

	mu       sync.RWMutex
	stopped  bool
	stopCh   chan struct{}
	canceled bool
	err      error
}
```

知りたいこと。たぶん以下の様な感じ

- waitGroupはgoroutineの同期
- RWMutexは内部の状態変数を触る時のlock
- stopChはキャンセルした時などに止まる(stopped=trueになりつつ。close(stopCh)する)
- `cancel()` は通常内部のcontextのキャンセル関数が格納

memo

- Stop(), Wait() と Go(), GoWithID()色々lockしたりされているけれど。名前の通り
- :notebook: contextから生成したいので、 `NewEnvironment()` も存在
- :notebook: stop()時点で新規のタスク生成早めて、wait()で全部のタスクを待つ感じでgraceful stop

### default.go

内部的にショートカット用のEnvironmentが生成されている。それを利用した関数が公開されている。

:notebook WithRequestIDで作られるidが良く分からない。 (lamport timestampみたいなやつ？)

```go
var (
	defaultEnv *Environment
)

func init() {
	defaultEnv = NewEnvironment(context.Background())
	handleSignal(defaultEnv)
}
```

### signal.go

:notebook: `handleSignal()` はsignal.goで定義されていて、なんかシグナルをキャッチしたら警告をログに出して。contextをcancelする。

### exec.go

そもそもexecのやつってなんなんだろ？ 標準ライブラリの `os/exec` をwrapしたものなのかな。

