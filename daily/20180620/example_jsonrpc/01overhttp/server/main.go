package main

import (
	"errors"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/rpc"
	"net/rpc/jsonrpc"
)

// Args :
type Args struct {
	A, B int
}

// Reply :
type Reply struct {
	C int
}

// Arith :
type Arith int

// Add :
func (t *Arith) Add(args *Args, reply *Reply) error {
	reply.C = args.A + args.B
	return nil
}

// Mul :
func (t *Arith) Mul(args *Args, reply *Reply) error {
	reply.C = args.A * args.B
	return nil
}

// Div :
func (t *Arith) Div(args *Args, reply *Reply) error {
	if args.B == 0 {
		return errors.New("divide by zero")
	}
	reply.C = args.A / args.B
	return nil
}

// Error :
func (t *Arith) Error(args *Args, reply *Reply) error {
	return errors.New("ERROR")
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

// fakeConnection :
type fakeConnection struct {
	io.ReadCloser
	io.Writer
}

func run() error {
	srv := rpc.NewServer()
	srv.Register(new(Arith))

	mux := http.DefaultServeMux
	mux.HandleFunc("/jsonrpc", func(w http.ResponseWriter, req *http.Request) {
		switch req.Method {
		case "CONNECT":
			conn, _, err := w.(http.Hijacker).Hijack()
			if err != nil {
				panic(err) // xxx
			}

			io.WriteString(conn, "HTTP/1.0 200 Connected to JSON RPC\n\n")
			srv.ServeCodec(jsonrpc.NewServerCodec(conn))
		case "POST":
			fconn := &fakeConnection{
				ReadCloser: ioutil.NopCloser(req.Body),
				Writer:     w,
			}
			srv.ServeCodec(jsonrpc.NewServerCodec(fconn))
		default:
			w.WriteHeader(404)
			io.WriteString(w, "404 Not found\n")
		}
	})
	return http.ListenAndServe(":4000", mux)
}
