package main

import (
	"fmt"
)

func main() {
	{
		fmt.Println("\u1D4D0")
	}

	{
		runes := []rune{0x1D4D0}
		fmt.Println(string(runes))
	}

	{
		var alphabets []rune
		for i := 0; i < 24; i++ {
			alphabets = append(alphabets, rune(0x1D4D0+i))
		}
		fmt.Println(string(alphabets))
	}
}
