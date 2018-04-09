package main

import "fmt2"

type s struct{}

func (s s) Println(x string) {}

func main() {
	fmt2.Println("xxx")

	{
		fmt := s{}
		fmt.Println("yyy")
	}

	fmt2.Println("xxx")
}
