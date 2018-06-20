package main

import (
	"fmt"
	"log"
	"net"
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

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	client, err := net.Dial("tcp", "127.0.0.1:1234")
	if err != nil {
		return err
	}

	args := &Args{7, 8}
	var reply Reply

	c := jsonrpc.NewClient(client)
	err = c.Call("Arith.Add", args, &reply)
	if err != nil {
		return err
	}
	fmt.Printf("Result: %d+%d=%d\n", args.A, args.B, reply)
	return nil
}
