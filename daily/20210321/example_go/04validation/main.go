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

func (r *Registry) RegisterType(rt reflect.Type) func([]string, interface{}) []*Error {
	if Debug {
		log.Printf("%-20s %v", "register type:", rt)
	}

	stringValidations := map[string][]func(string) error{}
	switch rt.Kind() {
	case reflect.Struct:
		for i := 0; i < rt.NumField(); i++ {
			rf := rt.Field(i)
			val, ok := rf.Tag.Lookup(r.Tag)
			if !ok {
				continue
			}
			args := strings.Split(strings.TrimSpace(val), ",")
			for _, x := range args {
				if strings.Contains(x, "=") {
					attrAndValue := strings.SplitN(x, "=", 2)
					attribute := strings.TrimSpace(attrAndValue[0])
					action := r.attributes[attribute]
					switch action.Type {
					case TargetTypeString:
						value := strings.TrimSpace(attrAndValue[1])
						fn := action.CreateStringValidation(value)
						stringValidations[rf.Name] = append(stringValidations[rf.Name], fn)
					default:
						panic(fmt.Sprintf("unexpected attribute: %s", attribute))
					}
				} else {
				}
			}
		}
	}

	fn := func(path []string, ob interface{}) []*Error {
		var errorList []*Error
		rv := reflect.ValueOf(ob)
		if rv.Kind() == reflect.Ptr {
			rv = rv.Elem()
		}
		for name, validations := range stringValidations {
			rf := rv.FieldByName(name)
			for _, v := range validations {
				if err := v(rf.String()); err != nil {
					errorList = append(errorList, &Error{Path: append(path, name), Message: err.Error()})
				}
			}
		}
		if len(errorList) == 0 {
			return nil
		}
		return errorList
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
	return &FullError{Code: 400, Message: "validateStruct", Errors: errorList}
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

type Error struct {
	Path    []string `json:"path"`
	Message string   `json:"message"`
}

func (e *Error) Error() string {
	return fmt.Sprintf(`{"path": %q, "message": %q}`, e.Path, e.Message)
}

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
	type Person struct {
		Name string `validate:"pattern=[A-Z]\\S+"`
	}
	// r.RegisterType(reflect.TypeOf(Person{}))
	v := &Validator{Registry: r}
	if err := v.ValidateStruct(Person{}); err != nil {
		enc := json.NewEncoder(os.Stderr)
		enc.SetIndent("", "  ")
		enc.Encode(err)
		log.Fatalf("!! %+v", err)
	}
}
