package main

import (
	"reflect"
	"unsafe"

	"github.com/pkg/errors"
)

type visit struct {
	a1  unsafe.Pointer
	typ reflect.Type
}

func walkValue(v1 reflect.Value, visited map[visit]bool, depth int) error {
	if !v1.IsValid() {
		return nil
	}
	// if depth > 10 { panic("walk") }	// for debugging

	hard := func(k reflect.Kind) bool {
		switch k {
		case reflect.Map, reflect.Slice, reflect.Ptr, reflect.Interface:
			return true
		}
		return false
	}

	if v1.CanAddr() && hard(v1.Kind()) {
		addr1 := unsafe.Pointer(v1.UnsafeAddr())
		typ := v1.Type()
		v := visit{addr1, typ}
		if visited[v] {
			return nil
		}
		visited[v] = true
	}

	switch v1.Kind() {
	case reflect.Array:
		for i := 0; i < v1.Len(); i++ {
			if err := walkValue(v1.Index(i), visited, depth+1); err != nil {
				return errors.WithMessagef(err, "%d", i)
			}
		}
		return nil
	case reflect.Slice:
		if v1.IsNil() {
			return nil
		}
		if v1.Len() == 0 {
			return nil
		}
		for i := 0; i < v1.Len(); i++ {
			if err := walkValue(v1.Index(i), visited, depth+1); err != nil {
				return errors.WithMessagef(err, "%d", i)
			}
		}
		return nil
	case reflect.Interface:
		if v1.IsNil() {
			return nil
		}
		return walkValue(v1.Elem(), visited, depth+1)
	case reflect.Ptr:
		if v1.IsNil() {
			return nil
		}
		return walkValue(v1.Elem(), visited, depth+1)
	case reflect.Struct:
		for i, n := 0, v1.NumField(); i < n; i++ {
			if err := walkValue(v1.Field(i), visited, depth+1); err != nil {
				return errors.WithMessagef(err, "%d", i) // field name
			}
		}
		return nil
	case reflect.Map:
		if v1.IsNil() {
			return nil
		}
		if v1.Len() == 0 {
			return nil
		}
		for _, k := range v1.MapKeys() {
			val1 := v1.MapIndex(k)
			if !val1.IsValid() {
				return nil
			}
			if err := walkValue(val1, visited, depth+1); err != nil {
				return errors.WithMessagef(err, "%s", k)
			}
		}
		return nil
	case reflect.Func:
		return nil // hmm
	default:
		return walkValue(reflect.ValueOf(v1), visited, depth)
	}
}
