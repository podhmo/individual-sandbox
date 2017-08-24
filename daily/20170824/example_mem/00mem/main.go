package main

import (
	"log"
	"net"
	"net/http"
	_ "net/http/pprof"
	"runtime"
	"time"
)

// DefaultPort is default port for profiler.
var DefaultPort = ":55555"

func setup() {
	runtime.SetBlockProfileRate(1)
	l, err := net.Listen("tcp", DefaultPort)
	if err != nil {
		l, err = net.Listen("tcp", "0.0.0.0:0")
		if err != nil {
			panic(err)
		}
	}
	go func() {
		log.Printf("start profiler: %s\n", l.Addr())
		if err := http.Serve(l, nil); err != nil {
			log.Fatal(err.Error())
		}
	}()
}

func main() {
	setup()
	time.Sleep(1 * time.Second)
}
