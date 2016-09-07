package main

import (
	"fmt"
)

func main() {
	s := "あいうえお"

	{
		fmt.Println(s)
	}
	{
		fmt.Println([]byte(s))
	}
	{
		fmt.Println([]rune(s))
	}
}
