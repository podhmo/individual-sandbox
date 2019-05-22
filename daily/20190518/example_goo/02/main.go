package main

import "fmt"

func main() {
	{
		ns := []int{010: 200, 005: 100}
		fmt.Println(ns)
		fmt.Println(len(ns))
	}
	{
		ns := []int{010: 200}
		fmt.Println(ns)
		fmt.Println(len(ns))
	}
}
