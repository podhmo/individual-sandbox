package main

import (
	"fmt"
	"log"
	"reflect"
	"strconv"
	"strings"

	"github.com/podhmo/validator/tag"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type S struct {
	X   string     `op:"@put"`
	XS  []string   `op:"@@put"`
	XS2 []string   `op:"@@put, @put"`
	XSS [][]string `op:"@@@put, @@put, @put"`
}

func run() error {
	s := tag.NewScannerDefault()
	s.Tag = "op"

	ob := S{
		X:   "Foo",
		XS:  []string{"iii", "jjj", "kkk"},
		XS2: []string{"xxx", "yyy", "zzz"},
		XSS: [][]string{[]string{"iii"}, []string{"jjj", "kkk", "yyy"}},
	}

	kludge, err := s.Scan(ob)
	if err != nil {
		return err
	}

	for _, code := range kludge.Code {
		fmt.Println(code.Addr, code.Op, "-", code.Args)
		if code.Op == tag.OpDeField {
			fmt.Println("")
		}
	}

	fmt.Println("----------------------------------------")
	return Eval(ob, kludge.Code)
}

func Eval(ob interface{}, code []*tag.Cell) error {
	if ob == nil {
		return fmt.Errorf("nil") // error?
	}

	root := reflect.ValueOf(ob)
	for root.Kind() == reflect.Ptr {
		root = root.Elem()
	}

	x := root
	pc := 0
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
		case tag.OpEnd:
			break loop
		case tag.OpField:
			x = root.FieldByName(cell.Args[0])
		case tag.OpDeField:
			fmt.Println("")
		// case tag.OpMap:
		// case tag.OpDeMap:
		case tag.OpSlice:
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
		case tag.OpDeSlice:
			f := stack[len(stack)-1]
			f.i++
			if f.i < f.n {
				pc = f.pc
				x = f.val
				x = x.Index(f.i)
			} else {
				stack = stack[:len(stack)-1] // pop
			}
		case tag.OpCall:
			fmt.Println(pc, strings.Repeat("  ", len(stack)), "--", cell.Args[0], x)
		default:
			return fmt.Errorf("unexpected opcode %+v", cell)
		}
		pc++
	}
	return nil
}
