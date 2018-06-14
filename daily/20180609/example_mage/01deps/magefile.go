// +build mage

package main

import (
	"fmt"

	"github.com/magefile/mage/mg"
)

func Build() {
	mg.Deps(F, G)
	fmt.Println("Build running")
}

func F() {
	mg.Deps(H)
	fmt.Println("f running")
}

func G() {
	mg.Deps(H)
	fmt.Println("g running")
}

func H() {
	fmt.Println("h running")
}
