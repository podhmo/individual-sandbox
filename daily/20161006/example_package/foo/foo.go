package foo

import (
	"fmt"
)

type Foo struct{}

func (f Foo) M() {
	fmt.Println("foo")
}
