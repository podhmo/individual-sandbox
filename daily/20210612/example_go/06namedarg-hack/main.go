package main

import (
	"database/sql"
	"fmt"
	"log"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	{
		arg := sql.NamedArg{Name: "x", Value: 1}
		fmt.Println("ok", arg)
	}
	// {
	// 	arg := sql.NamedArg{"x", 1}
	// 	fmt.Println("ng", arg)
	// }
	return nil
}
