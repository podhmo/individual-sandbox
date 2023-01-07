package main

import (
	"fmt"
	"go/token"
	"log"
	"reflect"
	"sort"
	"strings"

	"github.com/podhmo/individual-sandbox/daily/20230107/example_go/m"
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
	s.scan(rt, []string{rt.Name()})
}

func (s *Scanner) scan(rt reflect.Type, path []string) {
	switch rt.Kind() {
	case reflect.Bool:
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32:
	case reflect.Uint64:
	case reflect.Float32, reflect.Float64:
	case reflect.Slice, reflect.Array:
		s.scan(rt.Elem(), append(path, "[]"))
	case reflect.String:
	case reflect.Complex64, reflect.Complex128:
	case reflect.Chan:
	case reflect.Func:
	case reflect.Interface:
		log.Printf("skip: %s -- %v", rt.Name(), path)
	case reflect.Map:
		s.scan(rt.Key(), append(path, "k:"))
		s.scan(rt.Elem(), append(path, fmt.Sprintf("map[%s]", rt.Name())))
	case reflect.Struct:
		if _, ok := s.Sections[rt]; ok {
			return
		}
		s.scanStruct(rt, nil)
	case reflect.Pointer:
		s.scan(rt.Elem(), append(path, "*"))
		// case reflect.UnsafePointer:
	default:
		panic(fmt.Sprintf("unsupported: %v", rt.Kind()))
	}
}

func (s *Scanner) scanStruct(rt reflect.Type, path []string) {
	section := &Section{Name: rt.Name(), PkgPath: rt.PkgPath(), rt: rt, ID: len(s.Sections), Fields: []Field{}}
	s.Sections[rt] = section
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if !s.Config.IncludeUnexported && !token.IsExported(rf.Name) {
			continue
		}

		rft := rf.Type
		path := append(path, "F", rf.Name)
		field := Field{Name: rf.Name, Tag: rf.Tag}
		ops := []Op{}

		// TODO: embedded

	inner:
		for {
			switch rft.Kind() {
			case reflect.Pointer:
				ops = append(ops, Op{Type: OpTypePointer})
				path = append(path, "*")
			case reflect.Slice, reflect.Array:
				ops = append(ops, Op{Type: OpTypeSlice})
				path = append(path, "[]")
			case reflect.Map:
				ops = append(ops, Op{Type: OpTypeMap, S: rft.Key().Name()})
				path = append(path, fmt.Sprintf("map[%s]", rft.Key().Name()))
				// TODO: map, channel, generics
			default:
				break inner
			}
			rft = rft.Elem()
		}
		if rft.PkgPath() == "" { // builtin?
			ops = append(ops, Op{Type: OpTypeBasic})
		}

		if rft.Kind() == reflect.Interface {
			ops = append(ops, Op{Type: OpTypeAny})
			field.Type = Type{Name: "interface{}", ID: 0}
			field.Ops = ops
			section.Fields = append(section.Fields, field)
		} else {
			s.scan(rft, append(path, rt.Name()))
			name := rft.Name()
			id := -0
			undelying := ""
			if sub, ok := s.Sections[rft]; ok {
				id = sub.ID
			} else if v := rft.Kind().String(); name != v {
				undelying = v
			}
			field.Type = Type{Name: rft.Name(), ID: id, Underlying: undelying}
			field.Ops = ops
			section.Fields = append(section.Fields, field)
		}
	}
}

type Section struct {
	rt reflect.Type

	ID      int
	Name    string // todo: package
	PkgPath string
	Fields  []Field
}

type Field struct {
	Name string
	Tag  reflect.StructTag
	Type Type
	Ops  []Op
}

type Op struct {
	Type OpType
	S    string
}

type Type struct {
	Name       string
	ID         int
	Underlying string
}

/*
op:Type	S:Name
op:Pointer	S:-
*/

type OpType string

const (
	OpTypeBasic   OpType = "Basic"
	OpTypePointer OpType = "Pointer"
	OpTypeSlice   OpType = "Slice"
	OpTypeMap     OpType = "Map"
	OpTypeAny     OpType = "Any"
)

func main() {

	s := &Scanner{Sections: map[reflect.Type]*Section{}}
	s.Scan(m.Group{})
	// s.Scan(openapi3.T{})

	sections := make([]*Section, 0, len(s.Sections))
	for _, section := range s.Sections {
		sections = append(sections, section)
	}
	sort.SliceStable(sections, func(i, j int) bool { return sections[i].ID < sections[j].ID })
	for _, section := range sections {
		fmt.Printf("%02d:%s\t(%s)\n", section.ID, section.Name, section.PkgPath)
		for _, field := range section.Fields {
			fmt.Printf("\t%-10s", field.Name)
			var code []string
			for _, op := range field.Ops {
				code = append(code, fmt.Sprintf("%v", op))
			}
			if field.Type.Underlying != "" {
				code = append(code, fmt.Sprintf("%02d:%s(%s)", field.Type.ID, field.Type.Name, field.Type.Underlying))
			} else {
				code = append(code, fmt.Sprintf("%02d:%s", field.Type.ID, field.Type.Name))
			}
			fmt.Printf(" %-40s", strings.Join(code, " "))
			fmt.Println(" -- ", field.Tag)
		}
	}
}
