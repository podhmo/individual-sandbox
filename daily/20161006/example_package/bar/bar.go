package bar

import (
	. "../foo"
	"fmt"
)

type Bar Foo

type Bar2 struct {
	Foo
}

func init() {
	fmt.Println("from bar--------------------------------")
	Foo{}.M()
	fmt.Println("----------------------------------------")
}
