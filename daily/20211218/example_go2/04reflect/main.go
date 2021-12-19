package main

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

type S struct {
	Name string
	Foo  fmt.Stringer
}

type W struct {
	Foo string
	S
}

type W2 struct {
	S
	Foo string
}

func main() {
	// ok
	{
		fmt.Println("----------------------------------------")
		var ob S
		buf := strings.NewReader(`{"name": "foo"}`)
		fmt.Println(json.NewDecoder(buf).Decode(&ob))
		fmt.Println(json.NewEncoder(os.Stdout).Encode(ob))
	}

	// ng
	{
		fmt.Println("----------------------------------------")
		var ob S
		buf := strings.NewReader(`{"name": "foo", "foo": "xxx"}`)
		fmt.Println(json.NewDecoder(buf).Decode(&ob))
		fmt.Println(json.NewEncoder(os.Stdout).Encode(ob))
	}

	// ok
	{
		fmt.Println("----------------------------------------")
		var ob W
		buf := strings.NewReader(`{"name": "foo", "foo": "xxx"}`)
		fmt.Println(json.NewDecoder(buf).Decode(&ob))
		fmt.Println(json.NewEncoder(os.Stdout).Encode(ob))
	}

	// ok
	{
		fmt.Println("----------------------------------------")
		var ob W2
		buf := strings.NewReader(`{"name": "foo", "foo": "xxx"}`)
		fmt.Println(json.NewDecoder(buf).Decode(&ob))
		fmt.Println(json.NewEncoder(os.Stdout).Encode(ob))
	}
}
