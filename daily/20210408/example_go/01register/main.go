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
	registry map[string]ValidationFunc = map[string]ValidationFunc{}
	mu       sync.RWMutex
)

func Register(name string, fn ValidationFunc) error {
	defer mu.Unlock()
	mu.Lock()
	if _, existed := registry[name]; existed {
		return fmt.Errorf("validation %s is already registered, please use RegisterForce(), if you want to overwrite it.")
	}
	log.Printf("	register %q %v", name, fn)
	registry[name] = fn
	return nil
}
func RegisterForce(name string, fn ValidationFunc) {
	defer mu.Unlock()
	mu.Lock()
	log.Printf("	register %q %v", name, fn)
	registry[name] = fn
}

func Lookup(name string) ValidationFunc {
	defer mu.RUnlock()
	mu.RLock()
	fn := registry[name]
	log.Printf("	lookup %q %v", name, fn)
	return fn
}

func Validate(s *tagscan.Scanner, ob interface{}) error {
	if ob == nil {
		return fmt.Errorf("nil") // error?
	}

	kludge, err := s.Scan(ob)
	if err != nil {
		return err
	}

	root := reflect.ValueOf(ob)
	for root.Kind() == reflect.Ptr {
		root = root.Elem()
	}

	// fmt.Fprintln(os.Stderr, kludge.Describe())

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
loop:
	for {
		cell := code[pc]
		switch cell.Op {
		case tagscan.OpEnd:
			break loop
		case tagscan.OpField:
			x = root.FieldByName(cell.Args[0])
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

type Config struct {
	Color     string   `validate:"@regexp=^(#[0-9a-fA-F]{3}|)$"`
	ColorList []string `validate:"@@regexp=^#[0-9a-fA-F]{3}$"`
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
	s := tagscan.NewConfigDefault().Scanner()

	configList := []Config{
		{Color: "#999"},
		{Color: "#999x"},
		{ColorList: []string{"#999"}},
		{ColorList: []string{"#999x"}},
		{ColorList: []string{"#999"}},
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
