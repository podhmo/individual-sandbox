package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net"
	"net/rpc"
	"net/rpc/jsonrpc"

	"github.com/k0kubun/pp"
)

type Args struct {
	A, B int
}

type Reply struct {
	C int
}

type Arith int

type ArithAddResp struct {
	Id     interface{} `json:"id"`
	Result Reply       `json:"result"`
	Error  interface{} `json:"error"`
}

func (t *Arith) Add(args *Args, reply *Reply) error {
	reply.C = args.A + args.B
	return nil
}

func (t *Arith) Mul(args *Args, reply *Reply) error {
	reply.C = args.A * args.B
	return nil
}

func (t *Arith) Div(args *Args, reply *Reply) error {
	if args.B == 0 {
		return errors.New("divide by zero")
	}
	reply.C = args.A / args.B
	return nil
}

func (t *Arith) Error(args *Args, reply *Reply) error {
	panic("ERROR")
}

func init() {
	rpc.Register(new(Arith))
}

// ServeConn runs the JSON-RPC server on a single connection.
// ServeConn blocks, serving the connection until the client hangs up.
// The caller typically invokes ServeConn in a go statement.
func ServeConn(conn io.ReadWriteCloser) {
	rpc.ServeCodec(jsonrpc.NewServerCodec(conn))
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	cli, srv := net.Pipe()
	defer cli.Close()

	go ServeConn(srv)
	dec := json.NewDecoder(cli)

	// Send hand-coded requests to server, parse responses.
	for i := 0; i < 5; i++ {
		fmt.Fprintf(cli, `{"method": "Arith.Add", "id": "\u%04d", "params": [{"A": %d, "B": %d}]}`, i, i, i+1)
		var resp ArithAddResp
		err := dec.Decode(&resp)
		if err != nil {
			return err
		}
		pp.Println(resp)
	}
	return nil
}
