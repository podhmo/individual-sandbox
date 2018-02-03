package main

import (
	"fmt"

	"../model2"
)

func main() {
	u := model2.User2{Name: "foo", Age: 20}
	fmt.Printf("%#v\n", u)
	fmt.Println(u.Hello())
}
