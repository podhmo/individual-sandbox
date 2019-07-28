package reflectwalk

import (
	"fmt"
	"reflect"
	"strconv"

	"github.com/pkg/errors"
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
					return errors.WithMessagef(err, "%d", i)
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
				if err := walk(append(path, strconv.Itoa(i)), v.Index(i), callback); err != nil {
					return errors.WithMessagef(err, "%d", i)
				}
			}
			// predeclared type?
			if v.Type().NumMethod() == 0 {
				return nil
			}
			callback(path, v.Interface())
			return nil
		case reflect.Array:
			for i := 0; i < v.Len(); i++ {
				if err := walk(append(path, strconv.Itoa(i)), v.Index(i), callback); err != nil {
					return errors.WithMessagef(err, "%d", i)
				}
			}
			// predeclared type?
			if v.Type().NumMethod() == 0 {
				return nil
			}
			callback(path, v.Interface())
			return nil
		case reflect.Map:
			if v.IsNil() {
				return nil
			}
			for _, k := range v.MapKeys() {
				val := v.MapIndex(k)
				if !val.IsValid() {
					continue
				}
				kind := k.Kind()
				switch kind {
				case reflect.Ptr, reflect.Interface:
					if k.IsNil() {
						continue
					}
					k = k.Elem()
				}
				var nextKey string
				switch kind {
				case reflect.Bool:
					nextKey = strconv.FormatBool(k.Bool())
				case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
					nextKey = strconv.FormatInt(k.Int(), 10)
				case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
					nextKey = strconv.FormatUint(k.Uint(), 10)
				case reflect.Uintptr:
					panic("unexpected value") // xxx:
				case reflect.Float32, reflect.Float64:
					nextKey = strconv.FormatFloat(k.Float(), 'f', 3, 64)
				case reflect.String:
					nextKey = k.String()
				default:
					if k.Type().NumMethod() > 0 {
						if _, ok := k.Type().MethodByName("String"); ok {
							nextKey = k.MethodByName("String").Call(nil)[0].String()
						}
					}
					// default:
					// 	return fmt.Errorf("unexpected value (key) %s, %v, %+v", path, val.Kind(), val)
				}
				if nextKey == "" {
					continue // xxx:
				}

				// key
				// '@' is special marker (this is not good)
				if err := walk(append(path, nextKey, "@"), k, callback); err != nil {
					return errors.WithMessage(errors.WithMessage(err, "@"), nextKey)
				}
				// value
				if err := walk(append(path, nextKey), val, callback); err != nil {
					return errors.WithMessage(err, nextKey)
				}
			}
			// predeclared type?
			if v.Type().NumMethod() == 0 {
				return nil
			}
			callback(path, v.Interface())
			return nil
		// Invalid, Chan, Func, UnsafePointer
		default:
			return fmt.Errorf("unexpected value %s, %v, %+v", path, v.Kind(), v)
		}
	case interface{}:
		return walk(path, reflect.ValueOf(v), callback)
	default:
		return fmt.Errorf("unexpected value.. %s, %+v", path, v)
	}
}
