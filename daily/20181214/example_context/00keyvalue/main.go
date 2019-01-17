package main

import (
	"context"
	"fmt"
	"log"

	"./a"
	"./b"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctx := b.Set(a.Set(context.Background(), "xxx"), "yyy")
	fmt.Println(a.Get(ctx), b.Get(ctx))
	return nil
}
