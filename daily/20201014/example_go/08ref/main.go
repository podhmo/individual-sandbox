package main

import (
	"encoding/json"
	"fmt"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

func main() {
	// {
	// 	ob := "foo"
	// 	s := shape.Extract(&ob)
	// 	fmt.Printf("%+v %T\n", s, s)
	// 	b, _ := json.MarshalIndent(s, "", "  ")
	// 	fmt.Println(string(b))
	// 	fmt.Printf("%T**\n", s)
	// }
	{
		type S struct {
			Foo *string
		}
		ob := S{}
		s := shape.Extract(&ob)
		fmt.Printf("%+v %T\n", s, s)
		b, _ := json.MarshalIndent(s, "", "  ")
		fmt.Println(string(b))
		fmt.Printf("%T**\n", s.(shape.Struct).Fields.Values[0])
	}
	// {
	// 	type S struct {
	// 		Foo *string
	// 	}
	// 	s := shape.Extract(func(*S) {})
	// 	fmt.Printf("%+v %T\n", s, s)
	// 	b, _ := json.MarshalIndent(s, "", "  ")
	// 	fmt.Println(string(b))
	// 	fmt.Printf("%T**\n", s.(shape.Function).Params.Values[0].(shape.Struct).Fields.Values[0])
	// }
}
