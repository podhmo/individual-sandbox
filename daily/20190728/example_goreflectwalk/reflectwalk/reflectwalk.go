package reflectwalk

import (
	"fmt"
	"reflect"
	"strconv"
)

type Kind uint

const (
	Invalid Kind = iota
	// Bool
	// Int
	// Int8
	// Int16
	// Int32
	// Int64
	// Uint
	// Uint8
	// Uint16
	// Uint32
	// Uint64
	// Uintptr
	// Float32
	// Float64
	// Complex64
	// Complex128
	Array
	Chan
	Func
	Interface
	Map
	// Ptr
	Slice
	// String
	// Struct
	UnsafePointer
)

// ReflectWalk :
func ReflectWalk(
	v interface{},
	callback func(path []string, v interface{}),
) error {
	var path []string
	return walk(path, v, callback)
}

func walk(
	path []string,
	v interface{},
	callback func(path []string, v interface{}),
) error {
	switch v := v.(type) {
	case reflect.Value:
		switch v.Kind() {
		case reflect.Bool,
			reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
			reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
			reflect.Uintptr,
			reflect.Float32, reflect.Float64,
			reflect.String:

			// predeclared type?
			if v.Type().NumMethod() == 0 {
				return nil
			}
			callback(path, v.Interface())
			return nil
		case reflect.Ptr:
			if v.IsNil() {
				return nil
			}
			return walk(path, v.Elem(), callback)
		case reflect.Interface:
			if v.IsNil() {
				return nil
			}
			return walk(path, v.Elem(), callback)
		case reflect.Struct:
			t := v.Type()
			for i, n := 0, v.NumField(); i < n; i++ {
				ftype := t.Field(i)
				name := ftype.Tag.Get("json")
				if name == "-" {
					continue
				}
				if name == "" {
					name = ftype.Name
				}
				if err := walk(append(path, name), v.Field(i), callback); err != nil {
					return err
				}
			}
			// predeclared type?
			if v.Type().NumMethod() == 0 {
				return nil
			}
			callback(path, v.Interface())
			return nil
		case reflect.Slice:
			if v.IsNil() {
				return nil
			}
			for i := 0; i < v.Len(); i++ {
				walk(append(path, strconv.Itoa(i)), v.Index(i), callback)
			}
			// predeclared type?
			if v.Type().NumMethod() == 0 {
				return nil
			}
			callback(path, v.Interface())
			return nil
		default:
			return fmt.Errorf("hmm %s, %v, %+v", path, v.Kind(), v)
		}
	case interface{}:
		return walk(path, reflect.ValueOf(v), callback)
	default:
		return fmt.Errorf("hmm.. %s, %+v", path, v)
	}
}
