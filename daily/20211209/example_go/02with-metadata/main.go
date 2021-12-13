package main

import (
	"encoding/json"
	"fmt"
	"os"
)

type Person struct {
	Name string
	Age  int
}

type Concat struct {
	Head interface{}
	Tail interface{}
}

func (c *Concat) MarshalJSON() ([]byte, error) {
	head, err := json.Marshal(c.Head)
	if err != nil {
		return nil, err
	}
	tail, err := json.Marshal(c.Tail)
	if err != nil {
		return nil, err
	}
	return append(head[:len(head)-1], append([]byte{',', ' '}, tail[1:]...)...), nil
}

func main() {
	ob := &Person{Name: "foo", Age: 20}
	fmt.Println(json.NewEncoder(os.Stdout).Encode(ob))
	{
		var metadata struct{ Limit int }
		metadata.Limit = 10
		ob := &Concat{Head: ob, Tail: metadata}
		fmt.Println(json.NewEncoder(os.Stdout).Encode(ob))

		// hmm
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		fmt.Println(enc.Encode(ob))
	}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	fmt.Println(enc.Encode(ob))
}
