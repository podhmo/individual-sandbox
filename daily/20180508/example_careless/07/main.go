package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	// go-swaggerで出力されたコードでこういうものがあった
	var n *int64
	json.Unmarshal([]byte("10"), n)
	fmt.Println(n)

	fmt.Println("----------------------------------------")

	// correct
	var m int64
	json.Unmarshal([]byte("10"), &m)
	fmt.Println(m)
}
