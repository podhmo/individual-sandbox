package clientgen

import (
	"reflect"
	"runtime"
	"strings"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

func FileName(fn interface{}) string {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	filename, _ := rfunc.FileLine(rfunc.Entry())
	return filename
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
		return strings.Repeat("*", s.GetLv()) + strings.TrimPrefix(strings.TrimPrefix(s.GetPackage()+"."+s.GetName(), "."), "main.")
	}
}
