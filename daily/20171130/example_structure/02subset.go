package main

import (
	"encoding/json"
	"fmt"
)

// P :
type P struct {
	X int
	Y int
}

// Q :
type Q struct {
	X int
}

func main() {
	p := P{X: 10, Y: 20}

	{
		// q := Q(p) // error
	}

	{
		b, _ := json.Marshal(&p)
		var q Q
		json.Unmarshal(b, &q)
		fmt.Printf("p=%#v, q=%#v\n", p, q)
	}
}
