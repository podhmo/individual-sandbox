package hello

import "fmt"

// Hello ...
func Hello(name string, age int, who string) {
	fmt.Printf("%s(%d): hello %s\n", who, age, name)
}
