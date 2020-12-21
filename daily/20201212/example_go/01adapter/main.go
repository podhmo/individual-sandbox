package main

import (
	"fmt"
	"strings"
)

func Hello(name string) string {
	return fmt.Sprintf("Hello %s", name)
}

type HelloAdapter func(name string) string

func (f HelloAdapter) Adapt(name string) string {
	return strings.ToUpper(f(name))
}

func main() {
	name := "foo"
	fmt.Println(Hello(name))
	fmt.Println(HelloAdapter(Hello).Adapt(name))
	fmt.Println(HelloAdapter(Hello)(name))
}
