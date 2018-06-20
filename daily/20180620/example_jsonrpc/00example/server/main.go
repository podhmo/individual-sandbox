package main

import (
	"errors"
	"log"
	"net"
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
	rpc.Register(new(Arith))
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	var arith Arith
	server := rpc.NewServer()
	server.Register(&arith)
	server.HandleHTTP(rpc.DefaultRPCPath, rpc.DefaultDebugPath)
	l, err := net.Listen("tcp", ":1234")
	if err != nil {
		return err
	}
	for {
		conn, err := l.Accept()
		if err != nil {
			return err
		}

		go server.ServeCodec(jsonrpc.NewServerCodec(conn))
	}
}
