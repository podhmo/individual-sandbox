package main

import (
	"fmt"
	"runtime/debug"
	"strings"
)

func F(inner func(string)) func(string) {
	name := "F"
	return func(m string) {
		padding := strings.Repeat("  ", len(strings.Split(string(debug.Stack()), "\n")))
		fmt.Printf("%s%s start\n", padding, name)
		inner(m)
		fmt.Printf("%s%s end\n", padding, name)
	}
}

func G(inner func(string)) func(string) {
	name := "G"
	return func(m string) {
		padding := strings.Repeat("  ", len(strings.Split(string(debug.Stack()), "\n")))
		fmt.Printf("%s%s start\n", padding, name)
		inner(m)
		fmt.Printf("%s%s end\n", padding, name)
	}
}

func H(inner func(string)) func(string) {
	name := "H"
	return func(m string) {
		padding := strings.Repeat("  ", len(strings.Split(string(debug.Stack()), "\n")))
		fmt.Printf("%s%s start\n", padding, name)
		inner(m)
		fmt.Printf("%s%s end\n", padding, name)
	}
}

func FGH() func(string) {
	return func(m string) {
		name := "F"
		padding := strings.Repeat("  ", 1)
		fmt.Printf("%s%s start\n", padding, name)
		defer fmt.Printf("%s%s end\n", padding, name)
		{
			name := "G"
			padding := strings.Repeat("  ", 2)
			fmt.Printf("%s%s start\n", padding, name)
			defer fmt.Printf("%s%s end\n", padding, name)
			{
				name := "H"
				padding := strings.Repeat("  ", 3)
				fmt.Printf("%s%s start\n", padding, name)
				defer fmt.Printf("%s%s end\n", padding, name)
				{
					fmt.Println(m)
				}
			}
		}
	}
}

func main() {
	handler := F(G(H(func(m string) { fmt.Println(m) })))
	handler("Hello")
	fmt.Println("----------------------------------------")
	FGH()("Hello")
}
