// +build mage

package main

import (
	"fmt"
)

// Default target to run when none is specified
// If not set, running mage will list available targets
// var Default = Build

// echo bye
func Bye() {
	fmt.Println("bye")
}

// echo hello
func Hello() {
	fmt.Println("hello")
}

var Default = Hello
