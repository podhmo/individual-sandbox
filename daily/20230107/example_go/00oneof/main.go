package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"sync"
)

// https://swagger.io/docs/specification/data-models/oneof-anyof-allof-not/

type Tagged interface {
	Tag() string
}

type Dog struct {
	Bark  bool
	Breed string // enum: [Dingo, Husky, Retriever, Shepherd]
}

func (dog Dog) Tag() string { return "Dog" }

type Cat struct {
	Hunts bool
	Age   int
}

func (cat Cat) Tag() string { return "Cat" }

type Pet interface {
	Tagged
	pet()
}

func (o Dog) pet() {}
func (o Cat) pet() {}

type OneOf[T Tagged] struct { // Dog | Cat
	Value T // embedded is not supported, so...
}

func (ref OneOf[T]) MarshalJSON() ([]byte, error) {
	buf := pool.Get().(*bytes.Buffer)
	buf.Reset()
	if _, err := fmt.Fprintf(buf, `{"$type": %q,`, ref.Value.Tag()); err != nil {
		return nil, err
	}
	b, err := json.Marshal(ref.Value)
	if err != nil {
		return nil, err
	}
	if _, err := buf.Write(b[1:]); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

type inner struct {
	Type string `json:"$type"`
}

func (ref *OneOf[T]) UnmarshalJSON(b []byte) error {
	var ob inner
	if err := json.Unmarshal(b, &ob); err != nil {
		return err
	}
	val := reflect.New(registry[ob.Type]).Interface()
	if err := json.Unmarshal(b, &val); err != nil {
		return err
	}
	ref.Value = val.(T)
	return nil
}

var pool *sync.Pool
var registry map[string]reflect.Type

func init() {
	pool = &sync.Pool{
		New: func() interface{} {
			return new(bytes.Buffer)
		},
	}

	registry = map[string]reflect.Type{}
	for _, v := range []Tagged{Dog{}, Cat{}} {
		registry[v.Tag()] = reflect.TypeOf(v)
	}
}

func main() {
	type S struct {
		Pet OneOf[Pet]
	}

	{
		s := S{Pet: OneOf[Pet]{Value: &Dog{}}}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(s); err != nil {
			panic(err)
		}
	}
	fmt.Println("----------------------------------------")
	{
		code := `{"Pet": {"$type": "Dog", "Bark": true, "Breed": "Dingo"}}`
		dec := json.NewDecoder(bytes.NewBufferString(code))
		var s S
		if err := dec.Decode(&s); err != nil {
			panic(err)
		}
		fmt.Printf("%#+v\n", s)

		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(s); err != nil {
			panic(err)
		}

		switch pet := s.Pet.Value.(type) {
		case *Dog: // Dog?
			fmt.Println("d", pet)
		case *Cat: // Cat?
			fmt.Println("c", pet)
		default:
			fmt.Println("hmm", pet)
		}
	}
}
