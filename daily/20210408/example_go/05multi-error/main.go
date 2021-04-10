package main

import (
	"fmt"
	"log"
	"reflect"
	"regexp"
	"strconv"
	"sync"

	"github.com/k0kubun/pp"
	"github.com/podhmo/validator"
	"github.com/podhmo/validator/tagscan"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type ValidationFunc func(depth int, path []string, arg string, val reflect.Value) []*validator.Error

var (
	fieldValidationRegistry map[string]ValidationFunc       = map[string]ValidationFunc{}
	typeValidationRegistry  map[reflect.Type]ValidationFunc = map[reflect.Type]ValidationFunc{}

	mu sync.RWMutex
)

func Register(name string, fn ValidationFunc) error {
	defer mu.Unlock()
	mu.Lock()
	if _, existed := fieldValidationRegistry[name]; existed {
		return fmt.Errorf("validation %s is already registered, please use RegisterForce(), if you want to overwrite it.")
	}
	log.Printf("	register %q %v", name, fn)
	fieldValidationRegistry[name] = fn
	return nil
}
func RegisterForce(name string, fn ValidationFunc) {
	defer mu.Unlock()
	mu.Lock()
	log.Printf("	register %q %v", name, fn)
	fieldValidationRegistry[name] = fn
}
func Lookup(name string) ValidationFunc {
	defer mu.RUnlock()
	mu.RLock()
	fn := fieldValidationRegistry[name]
	log.Printf("		lookup %q %v", name, fn)
	return fn
}
func Scan(s *tagscan.Scanner, kludge *tagscan.Kludge) error {
	rt := kludge.Type
	for rt.Kind() == reflect.Ptr {
		rt = rt.Elem()
	}

	log.Printf("	register validation for %v", rt)
	mu.RLock()
	if _, ok := typeValidationRegistry[rt]; ok {
		mu.RUnlock()
		return nil
	}
	mu.RUnlock()

	mu.Lock()
	defer mu.Unlock()
	typeValidationRegistry[rt] = func(depth int, path []string, arg string, root reflect.Value) []*validator.Error {
		// fmt.Fprintln(os.Stderr, kludge.Describe())
		for root.Kind() == reflect.Ptr {
			root = root.Elem()
		}

		x := root
		pc := 0
		code := kludge.Code

		type frame struct {
			val  reflect.Value
			i    int
			n    int
			pc   int
			iter *reflect.MapIter
		}
		stack := []*frame{}
		var errorList []*validator.Error

	loop:
		for {
			cell := code[pc]
			// fmt.Fprintf(os.Stderr, "%s%3d: %-10s %+v	%+v	%+v\n", strings.Repeat("  ", depth), pc, cell.Op, cell.Args, x, stack)
			switch cell.Op {
			case tagscan.OpEnd:
				break loop
			case tagscan.OpField:
				x = root.FieldByName(cell.Args[0])
				path = append(path, ":field:", cell.Args[0])
			case tagscan.OpDeField:
				path = path[:len(path)-2]
				// fmt.Println("")
			case tagscan.OpPtr:
				if x.IsNil() {
				skip:
					for {
						switch code[pc].Op {
						case tagscan.OpDeField:
							break skip
						case tagscan.OpDeSlice:
							break skip
						case tagscan.OpDeMap:
							break skip
						default:
							pc++
						}
					}
					continue
				}
				x = x.Elem()
			case tagscan.OpStruct:
				typ := x.Type()
				mu.RLock()
				vfn, ok := typeValidationRegistry[typ]
				mu.RUnlock()
				if !ok {
					log.Printf("scan %v in %v", typ, kludge.Type)
					if _, err := s.Scan(x.Interface()); err != nil {
						errorList = append(errorList, &validator.Error{Code: 500, Path: path, Message: fmt.Sprintf("scan %+v", err)})
						break loop
					}
					mu.RLock()
					vfn, ok = typeValidationRegistry[typ]
					mu.RUnlock()
					if !ok {
						errorList = append(errorList, &validator.Error{Code: 500, Path: path, Message: fmt.Sprintf("something wrong in %+v", typ)})
						break loop
					}
				}
				if err := vfn(depth+1, append(path, ":struct", cell.Args[0]), cell.Args[0], x); err != nil {
					errorList = append(errorList, err...)
				}
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
					path = append(path, ":index:", "-1")
					continue
				} else {
					x = x.Index(0)
					path = append(path, ":index:", "0")
				}
			case tagscan.OpDeSlice:
				f := stack[len(stack)-1]
				f.i++
				if f.i < f.n {
					pc = f.pc
					x = f.val
					x = x.Index(f.i)
					path = append(path[:len(path)-1], strconv.Itoa(f.i))
				} else {
					stack = stack[:len(stack)-1] // pop
					path = path[:len(path)-2]
				}
			case tagscan.OpMap:
				iter := x.MapRange()
				f := &frame{
					val:  x,
					pc:   pc,
					iter: iter,
				}
				stack = append(stack, f) // push
				if x.IsNil() {
					i, _ := strconv.Atoi(cell.Args[0])
					pc = int(i)
					path = append(path, ":key:", "")
					continue
				} else {
					iter.Next()
					x = iter.Value()
					path = append(path, ":key:", iter.Key().String())
				}
			case tagscan.OpDeMap:
				f := stack[len(stack)-1]
				if f.iter.Next() {
					pc = f.pc
					x = f.iter.Value()
					path = append(path[:len(path)-1], f.iter.Key().String())
				} else {
					stack = stack[:len(stack)-1] // pop
					path = path[:len(path)-2]
				}
			case tagscan.OpCall:
				name := cell.Args[0]
				vfn := Lookup(name) // TODO: prevent runtime error
				if vfn == nil {
					errorList = append(errorList, &validator.Error{Code: 500, Path: path, Message: fmt.Sprintf("unexpected vfn, %q is not found", name)})
					break loop
				}
				err := vfn(depth, append(path, ":call:", name), cell.Args[1], x)
				if err != nil {
					errorList = append(errorList, err...)
				}
			default:
				errorList = append(errorList, &validator.Error{Code: 500, Path: path, Message: fmt.Sprintf("unexpected opcode %+v", cell)})
				break loop
			}
			pc++
		}
		if errorList != nil {
			return errorList
		}
		return nil
	}
	return nil
}

func Validate(s *tagscan.Scanner, ob interface{}) error {
	if ob == nil {
		return nil
	}

	// TODO: map,slice
	typ := reflect.TypeOf(ob)
	for typ.Kind() == reflect.Ptr {
		typ = typ.Elem()
	}

	mu.RLock()
	vfn, ok := typeValidationRegistry[typ]
	mu.RUnlock()
	if !ok {
		if _, err := s.Scan(ob); err != nil {
			return err
		}
		mu.RLock()
		vfn, ok = typeValidationRegistry[typ]
		mu.RUnlock()
		if !ok {
			return fmt.Errorf("something wrong in %v", typ)
		}
	}

	errorList := vfn(0, []string{":struct:", typ.String()}, "", reflect.ValueOf(ob))
	if errorList != nil {
		return NewFullError(errorList)
	}
	return nil
}

func NewFullError(errorList []*validator.Error) *validator.FullError {
	if len(errorList) == 0 {
		return nil
	}

	err := validator.NewFullError(errorList)
	for _, item := range err.Errors {
		var path []string
		for i := 0; i < len(item.Path); i += 2 {
			switch item.Path[i] {
			case ":struct:":
				continue
			case ":index:", ":key:":
				if len(path) == 0 {
					path = append(path, fmt.Sprintf("[%s]", item.Path[i+1]))
					continue
				}
				n := len(path) - 1
				path = append(path[:n], fmt.Sprintf("%s[%s]", path[n], item.Path[i+1]))
				continue
			case ":call:":
				path = append(path, "@"+item.Path[i+1])
			default:
				path = append(path, item.Path[i+1])
			}
		}
		item.Path = path
	}
	return err
}

func Regexp() ValidationFunc {
	// https://json-schema.org/understanding-json-schema/reference/regular_expressions.html
	m := map[string]*regexp.Regexp{}
	var mu sync.RWMutex

	return func(depth int, path []string, pattern string, val reflect.Value) []*validator.Error {
		mu.RLock()
		rx, ok := m[pattern]
		mu.RUnlock()
		if !ok {
			log.Printf("compile regexp %q", pattern)
			x, err := regexp.Compile(pattern)
			if err != nil {
				return []*validator.Error{{Code: 500, Path: path, Message: err.Error()}}
			}
			rx = x
			mu.Lock()
			m[pattern] = rx
			mu.Unlock()
		}
		if !rx.MatchString(val.String()) {
			return []*validator.Error{{Path: path, Message: fmt.Sprintf("pattern %s is not matched", rx), Value: val.String()}}
		}
		return nil
	}
}

type Config struct {
	Color     string `validate:"@regexp=^(#[0-9a-fA-F]{3}|)$"`
	SubConfig *ColorConfig
	Data      []map[string]ColorConfig
}

type ColorConfig struct {
	Color string `validate:"@regexp=^(#[0-9a-fA-F]{3}|)$"`
}

func run() error {
	Register("regexp", Regexp())
	c := tagscan.NewConfigDefault()
	c.DetectStruct = true
	c.OnScan = Scan
	s := c.Scanner()
	s.ScanAll(Config{})

	configList := []Config{
		{},
		{Color: "#999"},
		{Color: "#999x"},
		{Color: "#999", SubConfig: &ColorConfig{Color: "#999"}},
		{Color: "#999", SubConfig: &ColorConfig{Color: "#999x"}}, // validation
		{Color: "#999", Data: []map[string]ColorConfig{nil,
			{
				"aaa": ColorConfig{Color: "#333"},
				"bbb": ColorConfig{Color: "#333"},
			}, nil,
			{
				"xxx": ColorConfig{Color: "#999"},
				"yyy": ColorConfig{Color: "#999"},
			}, nil}},
		{Color: "#999", Data: []map[string]ColorConfig{nil,
			{
				"aaa": ColorConfig{Color: "#333"},
				"bbb": ColorConfig{Color: "#333x"},
			}, nil,
			{
				"xxx": ColorConfig{Color: "#999"},
				"yyy": ColorConfig{Color: "#999x"},
			}, nil}},
	}
	for _, c := range configList {
		fmt.Println("----------------------------------------")
		pp.Println(c)
		err := Validate(s, c)
		if err != nil {
			log.Printf("	!! %+v", err)
		} else {
			log.Printf("	ok")
		}
	}
	return nil
}
