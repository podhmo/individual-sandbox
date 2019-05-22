package main

import "fmt"

func main() {
	{
		a := &[...]int{1}
		fmt.Printf("%T, %v, %v, %v\n", a, cap(a), len(a), a[cap(a)-len(a)])
	}
	{
		a := []int{1}
		fmt.Printf("%T, %v, %v, %v\n", a, cap(a), len(a), a[cap(a)-len(a)])
	}
	{
		a := [4]int{1}
		fmt.Printf("%T, %v, %v, %v\n", a, cap(a), len(a), a[cap(a)-len(a)])
	}
	{
		a := &[4]int{1}
		fmt.Printf("%T, %v, %v, %v\n", a, cap(a), len(a), a[cap(a)-len(a)])
	}
}
