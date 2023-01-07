package main

import (
	"fmt"
	"go/token"
	"reflect"
	"strings"
)

type Config struct {
	IncludeUnexported bool
	// TODO: handling interface
}

type Scanner struct {
	Config   Config
	Sections map[reflect.Type]*Section
}

func (s *Scanner) Scan(ob interface{}) {
	rt := reflect.TypeOf(ob)
	s.scan(rt, nil)
}

func (s *Scanner) scan(rt reflect.Type, path []string) {
	for rt.Kind() == reflect.Pointer {
		rt = rt.Elem()
	}
	if _, ok := s.Sections[rt]; ok {
		return
	}

	switch rt.Kind() {
	case reflect.Bool:
		return
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32:
		return
	case reflect.Uint64:
		return
	case reflect.Float32, reflect.Float64:
		return
	case reflect.Slice:
		s.scan(rt.Elem(), append(path, "[]"))
	case reflect.String:
		return
	// case reflect.Complex64:
	// case reflect.Complex128:
	// case reflect.Array:
	// case reflect.Chan:
	// case reflect.Func:
	// case reflect.Interface:
	// case reflect.Map:
	// case reflect.Pointer:
	case reflect.Struct:
		s.scanStruct(rt, nil)
	// case reflect.UnsafePointer:
	default:
		panic(fmt.Sprintf("unsupported: %v", rt.Kind()))
	}
}

func (s *Scanner) scanStruct(rt reflect.Type, path []string) {
	section := &Section{Name: rt.Name()}
	s.Sections[rt] = section
	for i := 0; i < rt.NumField(); i++ {
		path := path
		rf := rt.Field(i)

		if !s.Config.IncludeUnexported && !token.IsExported(rf.Name) {
			continue
		}

		rft := rf.Type
		field := Field{Name: rf.Name, Tag: rf.Tag}
		var ops []Op

	inner:
		for {
			switch rft.Kind() {
			case reflect.Pointer:
				ops = append(ops, Op{Type: OpTypePointer})
				path = append(path, "*")
			case reflect.Slice, reflect.Array:
				ops = append(ops, Op{Type: OpTypeSlice})
				path = append(path, "[]")
				// TODO: map, channel, generics
			default:
				break inner
			}
			rft = rft.Elem()
		}
		ops = append(ops, Op{Type: OpTypeType, S: rft.Name()})
		field.Ops = ops
		section.Fields = append(section.Fields, field)
		s.scan(rft, append(path, rt.Name()))
	}
}

type Section struct {
	rt     reflect.Type
	Name   string // todo: package
	Fields []Field
}

type Field struct {
	Name string
	Tag  reflect.StructTag
	Ops  []Op
}

type Op struct {
	Type OpType
	S    string
}

/*
op:Type	S:Name
op:Pointer	S:-
*/

type OpType string

const (
	OpTypeType    OpType = "Type"
	OpTypePointer OpType = "Pointer"
	OpTypeSlice   OpType = "Slice"
)

func main() {
	type Person struct {
		Name     string `json:"name"`
		Age      int
		Nickname *string

		Father   *Person
		Children []Person
	}

	type Group struct {
		Members []Person
	}

	s := &Scanner{Sections: map[reflect.Type]*Section{}}
	s.Scan(Group{})

	for _, section := range s.Sections {
		fmt.Println(section.Name)
		for _, field := range section.Fields {
			fmt.Printf("\t%-10s", field.Name)
			var code []string
			for _, op := range field.Ops {
				code = append(code, fmt.Sprintf("%v", op))
			}
			fmt.Printf(" %-40s", strings.Join(code, " "))
			fmt.Println(" -- ", field.Tag)
		}
	}
}
