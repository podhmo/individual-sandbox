package main

import (
	"fmt"
	"log"
	"reflect"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Foo string

func use(ob interface{}) {
	fmt.Printf("%[1]T: %#+[1]v\n", ob.(string))
}
func use2(ob Foo) {
	fmt.Printf("%[1]T: %#+[1]v\n", string(ob))
}
func use3(ob interface{}) {
	fmt.Printf("%[1]T: %#+[1]v\n", reflect.ValueOf(ob).String())
	fmt.Println("	", reflect.ValueOf(ob).Kind())
}

func run() error {
	use("foo")
	// use(Foo("foo")) panic
	use2(Foo("foo"))
	use3("foo")
	use3(Foo("foo"))
	return nil
}
