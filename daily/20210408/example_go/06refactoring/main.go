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

type Registry struct {
	fieldValidationRegistry map[string]ValidationFunc
	typeValidationRegistry  map[reflect.Type]ValidationFunc

	mu sync.RWMutex
}

func (r *Registry) Register(name string, fn ValidationFunc) error {
	defer r.mu.Unlock()
	r.mu.Lock()
	if _, existed := r.fieldValidationRegistry[name]; existed {
		return fmt.Errorf("validation %s is already registered, please use RegisterForce(), if you want to overwrite it.")
	}
	log.Printf("	register field-validation @%s %v", name, fn)
	r.fieldValidationRegistry[name] = fn
	return nil
}
func (r *Registry) RegisterForce(name string, fn ValidationFunc) {
	defer r.mu.Unlock()
	r.mu.Lock()
	log.Printf("	register field-validation @%s %v", name, fn)
	r.fieldValidationRegistry[name] = fn
}
func (r *Registry) Lookup(name string) ValidationFunc {
	defer r.mu.RUnlock()
	r.mu.RLock()
	fn := r.fieldValidationRegistry[name]
	return fn
}

func (v *Validator) OnScan(s *tagscan.Scanner, kludge *tagscan.Kludge) error {
	rt := kludge.Type
	for rt.Kind() == reflect.Ptr {
		rt = rt.Elem()
	}

	log.Printf("	register type-validation of %v", rt)
	v.mu.RLock()
	if _, ok := v.typeValidationRegistry[rt]; ok {
		v.mu.RUnlock()
		return nil
	}
	v.mu.RUnlock()

	v.mu.Lock()
	defer v.mu.Unlock()
	v.typeValidationRegistry[rt] = func(depth int, path []string, arg string, root reflect.Value) []*validator.Error {
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
				v.mu.RLock()
				vfn, ok := v.typeValidationRegistry[typ]
				v.mu.RUnlock()
				if !ok {
					log.Printf("scan %v in %v", typ, kludge.Type)
					if _, err := s.Scan(x.Interface()); err != nil {
						errorList = append(errorList, &validator.Error{Code: 500, Path: path, Message: fmt.Sprintf("scan %+v", err)})
						break loop
					}
					v.mu.RLock()
					vfn, ok = v.typeValidationRegistry[typ]
					v.mu.RUnlock()
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
				vfn := v.Lookup(name) // TODO: prevent runtime error
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

type Validator struct {
	*Registry
	Scanner *tagscan.Scanner

	setups []func(*Validator) error
}

func (v *Validator) Validate(ob interface{}) error {
	if ob == nil {
		return nil
	}
	if v.setups != nil {
		setups := v.setups
		v.setups = nil
		for _, setup := range setups {
			if err := setup(v); err != nil {
				return NewFullError([]*validator.Error{{Code: 500, Message: fmt.Sprintf("on setup %+v", err), Value: ob}})
			}
		}
	}

	// TODO: map,slice
	typ := reflect.TypeOf(ob)
	for typ.Kind() == reflect.Ptr {
		typ = typ.Elem()
	}

	v.mu.RLock()
	vfn, ok := v.typeValidationRegistry[typ]
	v.mu.RUnlock()
	if !ok {
		if _, err := v.Scanner.Scan(ob); err != nil {
			return err
		}
		v.mu.RLock()
		vfn, ok = v.typeValidationRegistry[typ]
		v.mu.RUnlock()
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

func NewValidator(obs ...interface{}) *Validator {
	c := tagscan.NewConfigDefault()
	c.DetectStruct = true
	v := &Validator{
		Registry: &Registry{
			fieldValidationRegistry: map[string]ValidationFunc{},
			typeValidationRegistry:  map[reflect.Type]ValidationFunc{},
		},
	}
	c.OnScan = v.OnScan

	s := c.Scanner()
	v.Scanner = s

	v.setups = append(v.setups, func(v *Validator) error {
		for _, ob := range obs {
			if _, err := s.ScanAll(ob); err != nil {
				return err
			}
		}
		return nil
	})
	return v
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
	validator := NewValidator(Config{})
	validator.Register("regexp", Regexp())

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
		err := validator.Validate(c)
		if err != nil {
			log.Printf("	!! %+v", err)
		} else {
			log.Printf("	ok")
		}
	}
	return nil
}
