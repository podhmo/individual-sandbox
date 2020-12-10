// +build main

package main

import (
	"fmt"
	foo "m/00foo"
)

func main() {
	fmt.Println("hello", foo.Name)
}
