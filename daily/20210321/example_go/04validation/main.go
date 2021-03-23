package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"reflect"
	"regexp"
	"strconv"
	"strings"
	"sync"
)

// TODO: struct,slice,map

var Debug bool

func init() {
	if ok, _ := strconv.ParseBool(os.Getenv("DEBUG")); ok {
		Debug = true
	}
}

type AttributeFunc func(val string)

type TargetType string

const (
	TargetTypeString TargetType = "string"
)

type AttributeAction struct {
	Type                   TargetType
	CreateStringValidation func(val string) func(string) error
}

type Registry struct {
	Tag string

	attributes map[string]AttributeAction
	strings    map[string]map[string]func(string) error
	types      map[reflect.Type]func([]string, interface{}) []*Error

	mu sync.RWMutex
}

func (r *Registry) RegisterStringAttribute(name string, create func(value string) func(string) error) func(value string) func(string) error {
	if Debug {
		log.Printf("%-20s (string, %q)", "register attribute:", name)
	}

	r.mu.Lock()
	if _, ok := r.attributes[name]; ok {
		log.Printf("attribute %q is already used", name)
	}
	fnMap := map[string]func(string) error{}
	r.strings[name] = fnMap
	fn := func(value string) func(string) error {
		r.mu.RLock()
		fn, ok := fnMap[value]
		r.mu.RUnlock()
		if ok {
			return fn
		}

		if Debug {
			log.Printf("	%-20s attribute=%s, value=%q", "register validation:", name, value)
		}
		fn = create(value)
		r.mu.Lock()
		fnMap[value] = fn
		r.mu.Unlock()
		return fn
	}
	r.attributes[name] = AttributeAction{
		Type:                   TargetTypeString,
		CreateStringValidation: fn,
	}
	r.mu.Unlock()
	return fn
}

func (r *Registry) resolveArgs(tagString string) [][2]string {
	args := strings.Split(strings.TrimSpace(tagString), ",")
	var result [][2]string
	for _, x := range args {
		attrAndValue := strings.SplitN(x, "=", 2)
		attribute := strings.TrimSpace(attrAndValue[0])
		value := ""
		if strings.Contains(x, "=") {
			value = strings.TrimSpace(attrAndValue[1])
		}
		result = append(result, [2]string{attribute, value})
	}
	return result
}

func (r *Registry) RegisterType(rt reflect.Type, tagString ...string) func([]string, interface{}) []*Error {
	if Debug {
		log.Printf("%-20s %v", "register type:", rt)
	}

	var fn func(path []string, ob interface{}) []*Error
	r.mu.Lock()
	r.types[rt] = fn
	r.mu.Unlock()

	switch rt.Kind() {
	case reflect.String:
		var validations []func(string) error
		for _, x := range tagString {
			for _, pair := range r.resolveArgs(x) {
				attribute := pair[0]
				action := r.attributes[attribute]
				validations = append(validations, action.CreateStringValidation(pair[1]))
			}
		}
		fn = func(path []string, ob interface{}) []*Error {
			val := reflect.ValueOf(ob).String()
			var errorList []*Error
			for _, validation := range validations {
				if err := validation(val); err != nil {
					errorList = append(errorList, &Error{Path: path, Message: err.Error()})
				}
			}
			if len(errorList) == 0 {
				return nil
			}
			return errorList
		}
	case reflect.Struct:
		typedValidations := map[string][]func(path []string, ob interface{}) []*Error{}
		stringValidations := map[string][]func(string) error{}
		sliceValidations := map[string][]func(path []string, ob interface{}) []*Error{}
		mapValidations := map[string][]func(path []string, ob interface{}) []*Error{}

		for i := 0; i < rt.NumField(); i++ {
			rf := rt.Field(i)
			rft := rf.Type
			if rft.Kind() == reflect.Ptr {
				rft = rft.Elem()
			}

			r.mu.RLock()
			switch rft.Kind() {
			case reflect.Slice:
				rft = rft.Elem()
				if validation, ok := r.types[rft]; ok {
					sliceValidations[rf.Name] = append(sliceValidations[rf.Name], validation)
				}
			case reflect.Map:
				rft = rft.Elem()
				if validation, ok := r.types[rft]; ok {
					mapValidations[rf.Name] = append(mapValidations[rf.Name], validation)
				}
			default:
				if validation, ok := r.types[rft]; ok {
					typedValidations[rf.Name] = append(typedValidations[rf.Name], validation)
				}
			}
			r.mu.RUnlock()

			tagString, ok := rf.Tag.Lookup(r.Tag)
			if !ok {
				continue
			}
			for _, pair := range r.resolveArgs(tagString) {
				attribute := pair[0]
				action := r.attributes[attribute]
				switch action.Type {
				case TargetTypeString:
					stringValidations[rf.Name] = append(stringValidations[rf.Name], action.CreateStringValidation(pair[1]))
				}
			}
		}

		fn = func(path []string, ob interface{}) []*Error {
			var errorList []*Error
			rv := reflect.ValueOf(ob)
			if rv.Kind() == reflect.Ptr {
				rv = rv.Elem()
			}

			if len(sliceValidations) > 0 {
				for name, validations := range sliceValidations {
					rf := rv.FieldByName(name)
					typ := rf.Type()
					if typ.Kind() == reflect.Ptr {
						typ = typ.Elem()
						rf = rf.Elem()
					}
					if rf.IsNil() {
						continue
					}
					typ = typ.Elem() // slice[X] -> X

					newPath := append(path, name)
					isPtr := typ.Kind() == reflect.Ptr
					if isPtr {
						typ = typ.Elem()
					}
					for i := 0; i < rf.Len(); i++ {
						x := rf.Index(i)
						if isPtr {
							x = x.Elem()
						}
						iface := x.Interface()
						for _, v := range validations {
							if v == nil { // recursive definition
								v = r.types[typ]
							}
							if errs := v(append(newPath, strconv.Itoa(i)), iface); errs != nil {
								errorList = append(errorList, errs...)
							}
						}
					}
				}
			}

			if len(mapValidations) > 0 {
				for name, validations := range mapValidations {
					rf := rv.FieldByName(name)
					typ := rf.Type()
					if typ.Kind() == reflect.Ptr {
						typ = typ.Elem()
						rf = rf.Elem()
					}
					if rf.IsNil() {
						continue
					}
					typ = typ.Elem() // map[X] -> X

					newPath := append(path, name)
					isPtr := typ.Kind() == reflect.Ptr
					if isPtr {
						typ = typ.Elem()
					}

					for iter := rf.MapRange(); iter.Next(); {
						k := iter.Key()
						x := iter.Value()
						if isPtr {
							x = x.Elem()
						}
						iface := x.Interface()
						for _, v := range validations {
							if v == nil { // recursive definition
								v = r.types[typ]
							}
							if errs := v(append(newPath, fmt.Sprintf("%v", k.Interface())), iface); errs != nil {
								errorList = append(errorList, errs...)
							}
						}
					}
				}
			}

			if len(typedValidations) > 0 {
				for name, validations := range typedValidations {
					rf := rv.FieldByName(name)
					typ := rf.Type()
					if typ.Kind() == reflect.Ptr {
						typ = typ.Elem()
						if rf.IsNil() {
							continue
						}
						rf = rf.Elem()
					}
					newPath := append(path, name)
					iface := rf.Interface()
					for _, v := range validations {
						if v == nil { // recursive definition
							v = r.types[typ]
						}
						if errs := v(newPath, iface); errs != nil {
							errorList = append(errorList, errs...)
						}
					}
				}
			}

			if len(stringValidations) > 0 {
				for name, validations := range stringValidations {
					rf := rv.FieldByName(name)
					s := rf.String()
					for _, v := range validations {
						if err := v(s); err != nil {
							errorList = append(errorList, &Error{Path: append(path, name), Message: err.Error()})
						}
					}
				}
			}
			if len(errorList) == 0 {
				return nil
			}
			return errorList
		}
	default:
		panic(fmt.Sprintf("unexpected type %s kind=%s", rt, rt.Kind()))
	}

	r.mu.Lock()
	r.types[rt] = fn
	r.mu.Unlock()
	return fn
}

func NewRegistry(tag string) *Registry {
	return &Registry{
		Tag:        tag,
		attributes: map[string]AttributeAction{},
		strings:    map[string]map[string]func(string) error{},
		types:      map[reflect.Type]func([]string, interface{}) []*Error{},
	}
}

type Validator struct {
	Registry *Registry
}

func (v *Validator) ValidateStruct(ob interface{}) error {
	path := []string{}
	rt := reflect.TypeOf(ob)
	if rt.Kind() == reflect.Ptr {
		rt = rt.Elem()
	}
	v.Registry.mu.RLock()
	fn, ok := v.Registry.types[rt]
	v.Registry.mu.RUnlock()
	if !ok {
		fn = v.Registry.RegisterType(rt)
	}
	errorList := fn(path, ob)
	if len(errorList) == 0 {
		return nil
	}
	return &FullError{Code: 400, Message: "ValidateStruct", Errors: errorList}
}

func (v *Validator) ValidateSlice(ob interface{}) error {
	path := []string{}
	rt := reflect.TypeOf(ob)
	rv := reflect.ValueOf(ob)
	if rt.Kind() == reflect.Ptr {
		rt = rt.Elem()
		rv = rv.Elem()
	}
	if rt.Kind() != reflect.Slice {
		return &FullError{Code: 400, Message: "ValidateSlice", Errors: []*Error{{Message: "not slice", Path: path}}}
	}
	rt = rt.Elem()
	isPtr := rt.Kind() == reflect.Ptr
	if isPtr {
		rt = rt.Elem()
	}

	v.Registry.mu.RLock()
	fn, ok := v.Registry.types[rt]
	v.Registry.mu.RUnlock()
	if !ok {
		fn = v.Registry.RegisterType(rt)
	}

	var errorList []*Error
	for i := 0; i < rv.Len(); i++ {
		x := rv.Index(i)
		if isPtr {
			x = x.Elem()
		}
		if err := fn(append(path, strconv.Itoa(i)), x.Interface()); err != nil {
			errorList = append(errorList, err...)
		}
	}

	if len(errorList) == 0 {
		return nil
	}
	return &FullError{Code: 400, Message: "ValidateSlice", Errors: errorList}
}

func (v *Validator) ValidateMap(ob interface{}) error {
	path := []string{}
	rt := reflect.TypeOf(ob)
	rv := reflect.ValueOf(ob)
	if rt.Kind() == reflect.Ptr {
		rt = rt.Elem()
		rv = rv.Elem()
	}
	if rt.Kind() != reflect.Map {
		return &FullError{Code: 400, Message: "ValidateMap", Errors: []*Error{{Message: "not map", Path: path}}}
	}
	rt = rt.Elem()
	isPtr := rt.Kind() == reflect.Ptr
	if isPtr {
		rt = rt.Elem()
	}

	v.Registry.mu.RLock()
	fn, ok := v.Registry.types[rt]
	v.Registry.mu.RUnlock()
	if !ok {
		fn = v.Registry.RegisterType(rt)
	}

	var errorList []*Error
	for iter := rv.MapRange(); iter.Next(); {
		k := rv.Key()
		x := rv.Value()
		if isPtr {
			x = x.Elem()
		}
		if err := fn(append(path, fmt.Sprintf("%v", k.Interface())), x.Interface()); err != nil {
			errorList = append(errorList, err...)
		}
	}

	if len(errorList) == 0 {
		return nil
	}
	return &FullError{Code: 400, Message: "ValidateMap", Errors: errorList}
}

type FullError struct {
	Code    int      `json:"code"`
	Message string   `json:"message"`
	Errors  []*Error `json:"errors"`
}

func (e *FullError) Error() string {
	b, err := json.Marshal(e)
	if err != nil {
		return fmt.Sprintf(`{"code": 500, "message": %q}`, err)
	}
	return string(b)
}

type Path []string

func (l *Path) MarshalJSON() ([]byte, error) {
	if l == nil {
		return []byte(`""`), nil
	}
	return []byte(strconv.Quote(strings.Join(*l, "/"))), nil
}
func (l *Path) UnmarshalJSON(b []byte) error {
	*l = Path(strings.Split(string(b), "/"))
	return nil
}

type Error struct {
	Path    Path   `json:"path"`
	Message string `json:"message"`
}

func (e *Error) Error() string {
	return fmt.Sprintf(`{"path": %q, "message": %q}`, e.Path, e.Message)
}

type Name string

func main() {
	r := NewRegistry("validate")
	r.RegisterStringAttribute("pattern", func(value string) func(string) error {
		rx := regexp.MustCompile(value)
		return func(s string) error {
			if rx.MatchString(s) {
				return nil
			}
			return fmt.Errorf("%q is not matched (pattern=%s)", s, rx)
		}
	})
	r.RegisterStringAttribute("notzero", func(value string) func(string) error {
		return func(s string) error {
			if len(s) == 0 {
				return fmt.Errorf("%q is zero length", s)
			}
			return nil
		}
	})
	r.RegisterType(reflect.TypeOf(Name("")), `pattern=[A-Z]\S+`)

	type Person struct {
		Name     string `validate:"pattern=[A-Z]\\S+,notzero"`
		Name2    Name
		Name3    *Name
		Father   *Person
		Children []Person
		Data     map[string]Person
	}
	// r.RegisterType(reflect.TypeOf(Person{}))

	var name Name = "XXX"
	// ob := Person{Name: "foo", Name3: &name}
	// ob := Person{Name: "foo", Father: &Person{Name: "boo", Name3: &name}}
	ob := Person{
		Name:     "foo",
		Father:   &Person{Name: "boo", Name3: &name},
		Children: []Person{{Name: "XXX"}, {Name: "yyy"}},
		Data:     map[string]Person{"x": Person{Name: "xxx"}},
	}

	v := &Validator{Registry: r}
	if err := v.ValidateStruct(ob); err != nil {
		enc := json.NewEncoder(os.Stderr)
		enc.SetIndent("", "  ")
		enc.Encode(err)
		// log.Fatalf("!! %+v", err)
	}

	// if err := v.ValidateSlice([]*Person{&ob}); err != nil {
	// 	enc := json.NewEncoder(os.Stderr)
	// 	enc.SetIndent("", "  ")
	// 	enc.Encode(err)
	// 	// log.Fatalf("!! %+v", err)
	// }
}
