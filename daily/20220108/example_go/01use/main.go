package main

import (
	"encoding/json"
	"fmt"
	"m/parse"
	"os"
)

func main() {
	// p, err := parse.Parse(`{"name": ""}`)
	p, err := parse.Parse(`{"name": "foo"}`)
	if err != nil {
		panic(err)
	}
	fmt.Println(p)
	fmt.Println(p.Name)
	json.NewEncoder(os.Stdout).Encode(p)
}
