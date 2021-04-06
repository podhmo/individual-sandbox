package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"reflect"
	"strconv"

	"github.com/podhmo/validator/tagscan"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Person struct {
	Name string
	Age  int
}

func run() error {
	cfg := tagscan.NewConfigDefault()
	cfg.FullScan = true

	s := cfg.Scanner()
	kludges, err := s.ScanAll(Person{})
	if err != nil {
		return err
	}

	w := &Walker{Definitions: map[string]interface{}{}}
	for _, kludge := range kludges {
		fmt.Println(kludge.Describe())
		if err := w.Walk(kludge); err != nil {
			return err
		}
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	enc.Encode(map[string]interface{}{
		"components": map[string]interface{}{
			"schemas": w.Definitions,
		},
	})
	return nil
}

type Walker struct {
	Definitions map[string]interface{}
}

func (w *Walker) Walk(kludge *tagscan.Kludge) error {
	root := reflect.ValueOf(kludge.Value)
	for root.Kind() == reflect.Ptr {
		root = root.Elem()
	}

	x := root
	pc := 0
	code := kludge.Code

	props := map[string]interface{}{}
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
			props[cell.Args[0]] = map[string]string{"type": x.Type().String()}
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
	w.Definitions[root.Type().String()] = props
	return nil
}
