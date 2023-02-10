package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strings"

	"github.com/perimeterx/marshmallow"
)

// https://pkg.go.dev/github.com/perimeterx/marshmallow

type Person struct {
	Name    string            `json:"name,omitempty"`
	Age     int               `json:"age,omitempty"`
	Friends map[string]string `json:"-"`
}

func (p *Person) UnmarshalJSON(b []byte) error {
	// TODO: cache
	m, err := marshmallow.Unmarshal(b, p, marshmallow.WithExcludeKnownFieldsFromMap(true))
	if err != nil {
		return err
	}
	p.Friends = CoerceMap[string, string](m)
	return nil
}

func CoerceMap[K, V comparable](src map[K]any) map[K]V {
	dst := make(map[K]V, len(src))
	for k, v := range src {
		dst[k] = v.(V)
	}
	return dst
}

func main() {
	// additionalPeroperties: nickname
	s := `{"name": "foo", "age": 20, "nickname": "F"}`
	{
		var ob Person
		if err := json.NewDecoder(strings.NewReader(s)).Decode(&ob); err != nil {
			log.Fatalf("!! %+v", err)
		}
		fmt.Printf("with json.Decoder:\n\tob=%#+v\n", ob)
	}
	{
		var ob Person
		m, err := marshmallow.Unmarshal([]byte(s), &ob, marshmallow.WithExcludeKnownFieldsFromMap(true))
		if err != nil {
			log.Fatalf("!! %+v", err)
		}
		fmt.Printf("with marshmallow.Unmarshal:\n\tob=%#+v\n\tretval=%#+v\n", ob, m)
	}
}

// Output:
// with json.Decoder:
// 	ob=main.Person{Name:"foo", Age:20, Friends:map[string]string{"nickname":"F"}}
// with marshmallow.Unmarshal:
// 	ob=main.Person{Name:"foo", Age:20, Friends:map[string]string(nil)}
// 	retval=map[string]interface {}{"nickname":"F"}
