package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

type PingInput struct {
	Message string `json:"message" required:"true"`
	N       int    `json:"n" required:"false"`
}

func main() {
	s := shape.Extract(PingInput{}).(shape.Struct)
	var requiredFields []int
	var unrequiredFields []int

	for i, _ := range s.Fields.Keys {
		if ok, _ := strconv.ParseBool(s.Tags[i].Get("required")); ok {
			requiredFields = append(requiredFields, i)
		} else {
			unrequiredFields = append(unrequiredFields, i)
		}
	}
	fmt.Print("func Do(")
	var args []string
	for _, i := range requiredFields {
		name := s.Fields.Keys[i]
		args = append(args, fmt.Sprintf("%s %s", name, s.Fields.Values[i].GetReflectType()))
	}
	args = append(args, fmt.Sprintf("options ...%sOption", s.GetName()))
	fmt.Print(strings.Join(args, ", "))
	fmt.Println(") {")
	fmt.Println("}")
	fmt.Println("")

	fmt.Printf("type %sOption interface {\n", s.GetName())
	fmt.Printf("	Apply(*%s)\n", s.GetReflectType())
	fmt.Println("}")
	fmt.Println("")

	fmt.Printf("type %sOptionStruct struct {\n", s.GetName())
	for _, i := range unrequiredFields {
		name := s.Fields.Keys[i]
		f := s.Fields.Values[i]
		tag := s.Tags[i]
		fmt.Printf("	%s %s `%s`\n", name, f, tag)
	}
	fmt.Println("}")
}
