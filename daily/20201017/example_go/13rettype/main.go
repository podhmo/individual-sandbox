package main

import (
	"fmt"
	"m/13rettype/internal"
	"strings"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

func ListTodo() []internal.Todo { return nil }

func main() {
	s := shape.Extract(ListTodo).(shape.Function)
	fmt.Printf("%v: %s\n", s, TypeName(s.Returns.Values[0]))
}

func TypeName(s shape.Shape) string {
	switch s := s.(type) {
	case shape.Container:
		switch len(s.Args) {
		case 1:
			return "[]" + TypeName(s.Args[0])
		case 2:
			return strings.Repeat("*", s.GetLv()) + s.GetName() + "[" + TypeName(s.Args[0]) + "]" + TypeName(s.Args[1])
		default:
			panic("hmm")
		}
	default:
		parts := strings.Split(s.GetPackage(), "/")
		pkgName := parts[len(parts)-1]
		return strings.Repeat("*", s.GetLv()) + strings.TrimPrefix(strings.TrimPrefix(pkgName+"."+s.GetName(), "."), "main.")
	}
}
