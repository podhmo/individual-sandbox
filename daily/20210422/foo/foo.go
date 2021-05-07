//go:generate go run main_gen.go

package foo

import "fmt"

func Foo() {
	for _, x := range FOO_LIST {
		fmt.Println("foo", x)
	}
}
