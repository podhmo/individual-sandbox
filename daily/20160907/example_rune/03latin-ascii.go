package main

import (
	"fmt"
)

func main(){
    // a is code: U+0061, decimal: &#97
    // https://en.wikipedia.org/wiki/List_of_Unicode_characters#Basic_Latin
    fmt.Println(string([]rune("a")))
    fmt.Println([]rune("a"))
    fmt.Println("\u0061")

    fmt.Println(rune('a'))
}
