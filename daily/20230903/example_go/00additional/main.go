package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
	// additional properties
}

type Ref[T any] struct {
	Value *T
	raw   json.RawMessage
}

func (r *Ref[T]) UnmarshalJSON(b []byte) error {
	if r.Value == nil {
		var z T
		r.Value = &z
	}
	if err := json.Unmarshal(b, r.Value); err != nil {
		return err
	}
	r.raw = b
	return nil
}

func (r Ref[T]) String() string {
	return fmt.Sprintf("Ref[%T:%v]", r.Value, r.Value)
}

func main() {
	input := `{"name": "foo", "age": 20, "nickname": "F"}`
	var ob Ref[Person]
	if err := json.Unmarshal([]byte(input), &ob); err != nil {
		panic(err)
	}
	fmt.Println(ob)
}
