package main

import (
	"fmt"
	"log"
	"reflect"
	"regexp"
	"strconv"
	"sync"

	"github.com/podhmo/validator/tagscan"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type ValidationFunc func(arg string, val reflect.Value) error

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
	typeValidationRegistry[rt] = func(arg string, root reflect.Value) error {
		// fmt.Fprintln(os.Stderr, kludge.Describe())
		for root.Kind() == reflect.Ptr {
			root = root.Elem()
		}

		x := root
		pc := 0
		code := kludge.Code

		type frame struct {
			val reflect.Value
			i   int
			n   int
			pc  int
		}
		stack := []*frame{}
		for {
			cell := code[pc]
			switch cell.Op {
			case tagscan.OpEnd:
				return nil
			case tagscan.OpField:
				x = root.FieldByName(cell.Args[0])
			case tagscan.OpDeField:
			case tagscan.OpPtr:
				if x.IsNil() {
					for {
						if code[pc].Op == tagscan.OpDeField {
							break
						}
						pc++
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
						return err // xxx
					}
					mu.RLock()
					vfn, ok = typeValidationRegistry[typ]
					mu.RUnlock()
					if !ok {
						return fmt.Errorf(".. something wrong in %v", typ)
					}
				}
				if err := vfn(cell.Args[0], x); err != nil {
					return err
				}
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
				name := cell.Args[0]
				vfn := Lookup(name) // TODO: prevent runtime error
				if vfn == nil {
					return fmt.Errorf("unexpected vfn, %q is not found", name)
				}
				err := vfn(cell.Args[1], x)
				if err != nil {
					return err
				}
			default:
				return fmt.Errorf("unexpected opcode %+v", cell)
			}
			pc++
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
	return vfn("", reflect.ValueOf(ob))
}

type Config struct {
	Color     string `validate:"@regexp=^(#[0-9a-fA-F]{3}|)$"`
	SubConfig *ColorConfig
	Data      map[string]ColorConfig
}

type ColorConfig struct {
	Color string `validate:"@regexp=^(#[0-9a-fA-F]{3}|)$"`
}

func Regexp() func(string, reflect.Value) error {
	// https://json-schema.org/understanding-json-schema/reference/regular_expressions.html
	m := map[string]*regexp.Regexp{}
	var mu sync.RWMutex

	return func(pattern string, val reflect.Value) error {
		mu.RLock()
		rx, ok := m[pattern]
		mu.RUnlock()
		if !ok {
			log.Printf("compile regexp %q", pattern)
			x, err := regexp.Compile(pattern)
			if err != nil {
				return err
			}
			rx = x
			mu.Lock()
			m[pattern] = rx
			mu.Unlock()
		}
		if !rx.MatchString(val.String()) {
			return fmt.Errorf("%q is not matched (pattern=%s)", val, rx)
		}
		return nil
	}
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
	}
	for _, c := range configList {
		log.Printf("input %#+v", c)
		err := Validate(s, c)
		if err != nil {
			log.Printf("	!! %+v", err)
		} else {
			log.Printf("	ok")
		}
	}
	return nil
}
