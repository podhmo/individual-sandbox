package main

import (
	"fmt"
	"go/types"
	"log"
	"reflect"
	"sort"
	"strings"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(); err != nil {
		panic(err)
	}
}

func run() error {
	config := &packages.Config{
		Mode: packages.NeedTypes,
	}
	pkgs, err := packages.Load(config,
		"github.com/podhmo/individual-sandbox/daily/20230107/example_go/m",
		// 		"github.com/getkin/kin-openapi/openapi3",
	)
	if err != nil {
		return err
	}

	scanner := &Scanner{Sections: map[types.Type]*Section{}}
	for _, pkg := range pkgs {
		if len(pkg.Errors) > 0 {
			return fmt.Errorf("%v", pkg.Errors)
		}
		log.Println("pkg", pkg)

		s := pkg.Types.Scope()
		for _, name := range s.Names() {
			ob := s.Lookup(name)
			if _, ok := ob.Type().(*types.Named); ok {
				scanner.Scan(ob)
			}
		}
	}

	sections := make([]*Section, 0, len(scanner.Sections))
	for _, section := range scanner.Sections {
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

	// enc := json.NewEncoder(os.Stdout)
	// enc.SetIndent("", "  ")
	// if err := enc.Encode(sections); err != nil {
	// 	panic(err)
	// }

	return nil
}

type Config struct {
	IncludeUnexported bool
	// TODO: handling interface
}

type Scanner struct {
	Config   Config
	Sections map[types.Type]*Section
}

func (s *Scanner) Scan(ob types.Object) {
	t := ob.Type()
	s.scan(t, nil, []string{ob.Name()})
}

func (s *Scanner) scan(t types.Type, named *types.Named, path []string) {
	switch typ := t.(type) {
	case *types.Named:
		if !s.Config.IncludeUnexported && !typ.Obj().Exported() {
			return
		}
		s.scan(typ.Underlying(), typ, append(path, t.String()))
	case *types.Struct:
		if _, ok := s.Sections[named]; ok {
			return
		}
		s.scanStruct(typ, named, path)
	}
}

func (s *Scanner) scanStruct(t *types.Struct, named *types.Named, path []string) {
	name := named.Obj().Name()
	section := &Section{Name: name, PkgPath: named.Obj().Pkg().Path(), named: named, ID: len(s.Sections), Fields: []Field{}}
	s.Sections[named] = section

	for i := 0; i < t.NumFields(); i++ {
		f := t.Field(i)
		if !s.Config.IncludeUnexported && !f.Exported() {
			continue
		}

		field := Field{Name: f.Name(), Tag: reflect.StructTag(t.Tag(i))}
		typ := f.Type()
		ops := []Op{}
	inner:
		for {
			switch t := typ.(type) {
			case *types.Pointer:
				typ = t.Elem()
				ops = append(ops, Op{Type: OpTypePointer})
			case *types.Slice:
				typ = t.Elem()
				ops = append(ops, Op{Type: OpTypeSlice})
			case *types.Array:
				typ = t.Elem()
				ops = append(ops, Op{Type: OpTypeSlice})
			case *types.Map:
				typ = t.Elem()
				ops = append(ops, Op{Type: OpTypeMap, S: t.Key().String()})
			default:
				break inner
			}
		}

		switch t := typ.(type) {
		case *types.Basic:
			ops = append(ops, Op{Type: OpTypeBasic})
			id := 0
			name := t.Name()
			field.Type = Type{Name: name, ID: id}
			field.Ops = ops
			section.Fields = append(section.Fields, field)
		case *types.Named:
			s.scan(typ, nil, append(path, f.Name()))
			id := -0
			name := t.Obj().Name()
			underlying := ""
			if sub, ok := s.Sections[t]; ok {
				id = sub.ID
			} else {
				underlying = t.Underlying().String()
			}
			field.Type = Type{Name: name, ID: id, Underlying: underlying}
			field.Ops = ops
			section.Fields = append(section.Fields, field)
		case *types.Interface:
			if t.NumMethods() > 0 {
				log.Printf("skip: %v, %d", t, t.NumMethods())
				continue
			}
			ops = append(ops, Op{Type: OpTypeAny})
			field.Type = Type{Name: "interface{}", ID: 0}
			field.Ops = ops
			section.Fields = append(section.Fields, field)
		default:
			panic(fmt.Sprintf("unsupported: %v", typ))
		}
	}
}

type Section struct {
	named *types.Named

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
