package main

import (
	"fmt"
)

type barker interface {
	Bark()
}

type dog struct {
}

func (d *dog) Bark() {
	fmt.Println("\tわんわん")
}

func play(b barker) {
	fmt.Printf("\tplay with %[1]T: %[1]v\n", b, b)
	if b != nil {
		b.Bark()
	}
}

func main() {
	fmt.Println("```")
	{
		d := &dog{}
		fmt.Printf("1 nil? => %v\n", d == nil)
		play(d)
	}
	{
		var b barker
		fmt.Printf("2 nil? => %v\n", b == nil)
		play(b)
	}
	{
		var d *dog
		fmt.Printf("3 nil? => %v\n", d == nil)
		play(d)
	}
	fmt.Println("```")
}
