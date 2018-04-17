package main

import (
	"fmt"
	"log"
	"strconv"

	"github.com/k0kubun/pp"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func parse(x interface{}) float64 {
	n, err := strconv.ParseFloat(fmt.Sprintf("%v", x), 10)
	if err != nil {
		panic(err)
	}
	return n
}

func run() error {
	pp.Println(parse(int(10)))
	pp.Println(parse(int64(10)))
	pp.Println(parse(float64(10)))
	pp.Println(parse(float64(10.5)))
	return nil
}
