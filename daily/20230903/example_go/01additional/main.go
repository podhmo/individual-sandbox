package main

import (
	"encoding/json"
	"fmt"

	"github.com/perimeterx/marshmallow"
)

// https://github.com/PerimeterX/marshmallow

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

type Ref[T any] struct {
	Value       *T
	Additionals map[string]any
}

func (r *Ref[T]) UnmarshalJSON(b []byte) error {
	if r.Value == nil {
		var z T
		r.Value = &z
	}
	additionals, err := marshmallow.Unmarshal(b, r.Value, marshmallow.WithExcludeKnownFieldsFromMap(true))
	if err != nil {
		return err
	}
	r.Additionals = additionals
	return nil
}

// TODO: format-string
// TODO: pointer/value

func (r Ref[T]) String() string {
	keys := make([]string, 0, len(r.Additionals))
	for k := range r.Additionals {
		keys = append(keys, k)
	}
	return fmt.Sprintf("<Ref %T:%v, additionals=%s>", r.Value, r.Value, keys)
}

func main() {
	input := `{"name": "foo", "age": 20, "nickname": "F"}`
	var ob Ref[Person]
	if err := json.Unmarshal([]byte(input), &ob); err != nil {
		panic(err)
	}
	fmt.Println(ob)
	fmt.Println(&ob)
}
