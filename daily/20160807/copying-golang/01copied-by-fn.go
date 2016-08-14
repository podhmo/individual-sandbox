package main

import (
	"fmt"
)

type Foo struct {
	name string
}

func foo(foo Foo) {
	fmt.Printf("%p %s\n", &foo, foo.name)
}
func foo2(foo *Foo) {
	fmt.Printf("%p %s\n", foo, foo.name)
}

func main() {
	target := Foo{name: "foo"}
	fmt.Printf("original: %p\n", &target)
	fmt.Println("- foo ---------------------------------------")
	foo(target)
	foo(target)
	foo(target)
	fmt.Println("- foo2 ---------------------------------------")
	foo2(&target)
	foo2(&target)
	foo2(&target)
}
