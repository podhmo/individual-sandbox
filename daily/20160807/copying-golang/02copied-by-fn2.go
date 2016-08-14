// type aliases
package main

import (
	"fmt"
)

type Foo string

func foo(foo Foo) {
	fmt.Printf("%p %s\n", &foo, foo)
}

func foo2(foo *Foo) {
	fmt.Printf("%p %s\n", foo, *foo)
}

func main() {
	target := Foo("foo")
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
