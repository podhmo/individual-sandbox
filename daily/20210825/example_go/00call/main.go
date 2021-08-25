package main

import (
	"fmt"
)

type Call struct {
	Name string
}

func (c *Call) String() string {
	return fmt.Sprintf("%s()", c.Name)
}

func main() {
	foo := &Call{Name: "Foo"}
	fmt.Println(foo)
}
