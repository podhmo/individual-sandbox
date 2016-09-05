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

- default.go
- env.go
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
- signal.go
- stopsig.go
- stopsig_windows.go

