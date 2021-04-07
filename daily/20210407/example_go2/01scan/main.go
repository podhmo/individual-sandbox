package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"reflect"
	"strconv"
	"strings"

	"github.com/getkin/kin-openapi/openapi3"
	"github.com/podhmo/validator/tagscan"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Person struct {
	Name     string    `json:"name" validate:"@foo"`
	Age      int       `json:"age"`
	Children []*Person `json:"children"`
}

func run() error {
	cfg := tagscan.NewConfigDefault()
	cfg.FullScan = true

	s := cfg.Scanner()
	kludges, err := s.ScanAll(Person{})
	if err != nil {
		return err
	}

	components := openapi3.NewComponents()
	components.Schemas = openapi3.Schemas{}
	w := &Walker{
		Components: components,
		Resolver:   &Resolver{},
	}
	for _, kludge := range kludges {
		fmt.Println(kludge.Describe())
		if err := w.Walk(kludge); err != nil {
			return err
		}
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(w.Components)
	return nil
}

type Walker struct {
	Components openapi3.Components
	Resolver   *Resolver
}
type Resolver struct {
}

func (r *Resolver) Title(rt reflect.Type) string {
	// TODO: prevent conflict
	name := rt.String()
	return name[strings.LastIndex(name, ".")+1:]
}

func (w *Walker) Walk(kludge *tagscan.Kludge) error {
	root := reflect.ValueOf(kludge.Value)
	for root.Kind() == reflect.Ptr {
		root = root.Elem()
	}

	x := root
	pc := 0
	code := kludge.Code

	s := openapi3.NewSchema()
	s.Properties = openapi3.Schemas{}
	w.Components.Schemas[w.Resolver.Title(kludge.Type)] = openapi3.NewSchemaRef("", s)
	type frame struct {
		val reflect.Value
		i   int
		n   int
		pc  int
	}
	stack := []*frame{}
loop:
	for {
		cell := code[pc]
		switch cell.Op {
		case tagscan.OpEnd:
			break loop
		case tagscan.OpField:
			x = root.FieldByName(cell.Args[0])
			name := cell.Args[1] // json tag
			if name == "" {
				name = cell.Args[0]
			}
			s.Properties[name] = openapi3.NewSchemaRef("", openapi3.NewStringSchema())
		case tagscan.OpDeField:
			fmt.Println("")
		// case tagscan.OpMap:
		// case tagscan.OpDeMap:
		case tagscan.OpSlice:
			f := &frame{
				val: x,
				i:   0,
				n:   x.Len(),
				pc:  pc,
			}
			stack = append(stack, f) // push
			if x.IsNil() {
				i, _ := strconv.Atoi(cell.Args[0])
				pc = int(i)
			} else {
				x = x.Index(0)
			}
		case tagscan.OpDeSlice:
			f := stack[len(stack)-1]
			f.i++
			if f.i < f.n {
				pc = f.pc
				x = f.val
				x = x.Index(f.i)
			} else {
				stack = stack[:len(stack)-1] // pop
			}
		case tagscan.OpCall:
		default:
			return fmt.Errorf("unexpected opcode %+v", cell)
		}
		pc++
	}

	fmt.Printf("%[1]T: %+[1]v\n", kludge.Value)
	return nil
}
