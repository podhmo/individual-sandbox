package main

import "fmt"
import "net/url"

func main() {
	fmt.Println("…", "->", url.QueryEscape("…"))
	fmt.Println("-", "->", url.QueryEscape("-"))
	fmt.Println("_", "->", url.QueryEscape("_"))
	fmt.Println("/", "->", url.QueryEscape("/"))
}
