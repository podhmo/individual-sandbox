package main

import (
	"encoding/json"
	"os"
	"reflect"
)

type S struct {
	Name string `json:"name"`
}

func Wrapped(ob interface{}) interface{} {
	// TODO: support pointer
	rt := reflect.TypeOf(ob)
	wrappedType := reflect.StructOf([]reflect.StructField{
		reflect.StructField{
			Name: "TypeName",
			Type: reflect.TypeOf(""),
			Tag:  `json:"typeName"`,
		},
		reflect.StructField{
			Name:      rt.Name(),
			Type:      rt,
			Anonymous: true,
		},
	})
	rob := reflect.New(wrappedType).Elem()
	rob.Field(0).SetString(rt.Name())
	rob.Field(1).Set(reflect.ValueOf(ob))
	return rob.Interface()
}

func main() {
	{
		type W struct {
			TypeName string `json:"typename"`
			S
		}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		enc.Encode(W{TypeName: "S", S: S{Name: "foo"}})
	}
	{
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		enc.Encode(Wrapped(S{Name: "foo"}))
	}
}
