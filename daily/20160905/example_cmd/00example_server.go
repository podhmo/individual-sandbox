package example_server

import (
	"flag"
	"github.com/cybozu-go/cmd"
	"github.com/cybozu-go/log"
	"net/http"
	"syscall"
)

func Example_http() {
	flag.Parse() // must precedes LogConfig.Apply
	cmd.LogConfig{}.Apply()

	// log accesses to another file in JSON Lines format.
	w, err := log.NewFileReopener("/path/to/access.log", syscall.SIGUSR1)
	if err != nil {
		log.ErrorExit(err)
	}
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

	// ListenAndServe is overridden to start a goroutine by cmd.Go.
	err = serv.ListenAndServe()
	if err != nil {
		log.ErrorExit(err)
	}

	// Wait waits for SIGINT or SIGTERM.
	// In this case, cmd.Stop can be omitted.
	err = cmd.Wait()

	// Use IsSignaled to determine err is the result of a signal.
	if err != nil && !cmd.IsSignaled(err) {
		log.ErrorExit(err)
	}
}
