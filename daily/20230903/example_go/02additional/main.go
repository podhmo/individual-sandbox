package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"

	"github.com/perimeterx/marshmallow"
)

type WithAdditionals[T any] struct {
	Value       T
	Additionals map[string]any
}

// TODO: performance

func (r *WithAdditionals[T]) UnmarshalJSON(b []byte) error {
	additionals, err := marshmallow.Unmarshal(b, &r.Value, marshmallow.WithExcludeKnownFieldsFromMap(true))
	if err != nil {
		return err
	}
	r.Additionals = additionals
	return nil
}

func (r *WithAdditionals[T]) MarshalJSON() ([]byte, error) {
	var buf bytes.Buffer
	enc := json.NewEncoder(&buf)
	if err := enc.Encode(&r.Value); err != nil {
		return nil, err
	}
	if len(r.Additionals) == 0 {
		return buf.Bytes(), nil
	}

	pos := buf.Len() - 1 // joint point
	if err := enc.Encode(r.Additionals); err != nil {
		return nil, err
	}
	b := buf.Bytes()

	// `{"x":"k"}\n{"y": "v"}` => `{"x":"k" , "y": "v"}`
	b[pos-1] = ' '
	b[pos] = ','
	b[pos+1] = ' '
	return b, nil
}

type person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`

	Father *Person `json:"father"`
}
type Person = WithAdditionals[person]

func main() {
	fmt.Println("----------------------------------------")
	fmt.Println("decoding")
	fmt.Println("----------------------------------------")
	{
		input := `{"name": "foo", "age": 20}`
		fmt.Println("input:", input)
		var ob Person
		if err := json.Unmarshal([]byte(input), &ob); err != nil {
			panic(err)
		}
		fmt.Println("output:", ob)
	}
	fmt.Println("-")
	{
		input := `{"name": "foo", "age": 20, "nickname": "F"}`
		fmt.Println("input:", input)
		var ob Person
		if err := json.Unmarshal([]byte(input), &ob); err != nil {
			panic(err)
		}
		fmt.Println("output:", ob)
	}
	fmt.Println("-")
	{
		input := `{"name": "foo", "age": 20, "nickname": "F", "father": {"name": "moo", "nickname":"M"}}`
		fmt.Println("input:", input)
		var ob Person
		if err := json.Unmarshal([]byte(input), &ob); err != nil {
			panic(err)
		}
		fmt.Println("output:", ob)
	}
	fmt.Println("----------------------------------------")
	fmt.Println("encoding")
	fmt.Println("----------------------------------------")
	{
		var ob Person
		ob.Value.Name = "foo"
		ob.Value.Age = 20

		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(&ob); err != nil {
			panic(err)
		}
	}
	fmt.Println("-")
	{
		var ob Person
		ob.Value.Name = "foo"
		ob.Value.Age = 20
		ob.Additionals = map[string]any{}

		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(&ob); err != nil {
			panic(err)
		}
	}
	fmt.Println("-")
	{
		var ob Person
		ob.Value.Name = "foo"
		ob.Value.Age = 20
		ob.Additionals = map[string]any{"nickname": "F"}

		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(&ob); err != nil {
			panic(err)
		}
	}
	fmt.Println("-")
	{
		var ob Person
		ob.Value.Name = "foo"
		ob.Value.Age = 20
		ob.Additionals = map[string]any{"nickname": "F"}
		var father Person
		ob.Value.Father = &father
		father.Value.Name = "moo"
		father.Additionals = map[string]any{"nickname": "M"}

		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(&ob); err != nil {
			panic(err)
		}
	}
}
