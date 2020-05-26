package main

import (
	"flag"
	"log"
	"net/http"
	"os"

	"m/04jsonrpc/internal"

	"github.com/semrush/zenrpc"
)

func main() {
	addr := flag.String("addr", "localhost:9999", "listen address")
	flag.Parse()

	rpc := zenrpc.NewServer(zenrpc.Options{ExposeSMD: true})
	// rpc.Register("arith", internal.ArithService{})
	rpc.Register("", internal.ArithService{}) // public
	rpc.Use(zenrpc.Logger(log.New(os.Stderr, "", log.LstdFlags)))

	http.Handle("/", rpc)

	log.Printf("starting arithsrv on %s", *addr)
	log.Fatal(http.ListenAndServe(*addr, nil))
}
