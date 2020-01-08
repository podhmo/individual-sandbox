package main

import "fmt"

func main() {
	xMapMap := map[string]map[string]int{}
	xMap, ok := xMapMap["foo"]
	fmt.Println(xMap, ok)
	fmt.Println(xMapMap["foo"])
}
