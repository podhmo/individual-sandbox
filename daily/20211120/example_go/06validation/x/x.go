package x

import (
	"context"
	"fmt"
	"m/runtime"
	v "m/validation"
	"reflect"
	"strconv"
)

type Person struct {
	Name     string   `json:"name"`
	Father   *Person  `json:"father`
	Children []Person `json:"children"`
}

// Validation is marker method for code-generation
func (p Person) Validation() []v.FieldValidation {
	return []v.FieldValidation{
		v.Field("Name", v.Required()),
	}
}

// func (p Person) Validate(ctx context.Context, prefix string) *ErrorSet {
// 	fmt.Println("hai", prefix, p)
// 	return nil
// }

type Validator struct {
	Translator *Translator
	// ValidateStructFunc func(ctx context.Context, ob interface{}, prefix string) *ErrorSet
}

func (v *Validator) Validate(ctx context.Context, ob interface{}) error {
	s := &State{
		T:             v.Translator,
		MaxErrorItems: 100,
	}
	perrs, _ := s.Validate(ctx, ob, "")
	return perrs.NilOrError()
}

type State struct {
	T             *Translator
	MaxErrorItems int
}

func (v *State) Validate(ctx context.Context, ob interface{}, prefix string) (_ *ErrorSet, stop bool) {
	rv := reflect.ValueOf(ob)
	switch rv.Kind() {
	case reflect.Ptr, reflect.Struct:
		return v.Struct(ctx, ob, prefix)
	case reflect.Slice, reflect.Array:
		return v.Slice(ctx, rv, prefix)
	case reflect.Map:
		return v.Map(ctx, rv, prefix)
	case reflect.String: // TODO: more primitive type
	}
	return v.NewInternalError(fmt.Sprintf("hmm %v, %v", rv.Kind(), rv)), true // xxx
}

func (v *State) NewInternalError(msg string) *ErrorSet {
	return &ErrorSet{
		&FieldError{Translator: v.T, Value: msg},
	}
}
func (v *State) NewTooManyErrors(perrs *ErrorSet, path string) *ErrorSet {
	fe := &FieldError{Translator: v.T, Value: fmt.Sprintf("too many errors (len(errs) > %d) ...", v.MaxErrorItems)}
	*perrs = append(*perrs, fe)
	return perrs
}

func (v *State) Slice(ctx context.Context, rv reflect.Value, prefix string) (_ *ErrorSet, stop bool) {
	// TODO: primitive slice
	rt := rv.Type().Elem()
	isPtr := rt.Kind() == reflect.Ptr
	if isPtr {
		rt = rt.Elem()
	}

	rctx := reflect.ValueOf(ctx)
	perrs := new(ErrorSet)
	var rfn reflect.Value

	switch rt.Kind() {
	case reflect.Ptr, reflect.Struct:
		// return s.Slice(ctx, ob, prefix).NilOrError()
		rfn = reflect.ValueOf(v.Struct)
	case reflect.Slice, reflect.Map, reflect.Array:
		rfn = reflect.ValueOf(v.Validate)
	default:
		return v.NewInternalError(fmt.Sprintf("hmm slice.. %v, %v", rv.Kind(), rv)), true // xxx
	}

	for i, n := 0, rv.Len(); i < n; i++ {
		x := rv.Index(i)
		if isPtr && x.IsNil() {
			continue
		}
		ret := rfn.Call([]reflect.Value{rctx, x, reflect.ValueOf(prefix + strconv.Itoa(i) + ".")})
		if ret[0].IsNil() {
			continue
		}
		*perrs = append(*perrs, *(ret[0].Interface().(*ErrorSet)))
		if stop := ret[1].Bool(); stop {
			return perrs, stop
		}
		if len(*perrs) > v.MaxErrorItems {
			return v.NewTooManyErrors(perrs, prefix), true
		}
	}
	return perrs, false
}

func (v *State) Map(ctx context.Context, rv reflect.Value, prefix string) (_ *ErrorSet, stop bool) {
	// TODO: nested map
	// MEMO: handling JSON like structure, so key is always primitive type
	iter := rv.MapRange()
	rt := rv.Type().Elem() // type of value of map[<k>]

	isPtr := rt.Kind() == reflect.Ptr
	if isPtr {
		rt = rt.Elem()
	}

	rctx := reflect.ValueOf(ctx)
	perrs := new(ErrorSet)
	var rfn reflect.Value

	switch rt.Kind() {
	case reflect.Ptr, reflect.Struct:
		// return s.Slice(ctx, ob, prefix).NilOrError()
		rfn = reflect.ValueOf(v.Struct)
	case reflect.Slice, reflect.Map, reflect.Array:
		rfn = reflect.ValueOf(v.Validate)
	default:
		return v.NewInternalError(fmt.Sprintf("hmm slice.. %v, %v", rv.Kind(), rv)), true // xxx
	}

	for iter.Next() {
		rk := iter.Key()
		x := iter.Value()
		if isPtr && x.IsNil() {
			continue
		}
		ret := rfn.Call([]reflect.Value{rctx, x, reflect.ValueOf(prefix + fmt.Sprintf("%s", rk) + ".")})
		if ret[0].IsNil() {
			continue
		}
		*perrs = append(*perrs, *(ret[0].Interface().(*ErrorSet)))
		if stop := ret[1].Bool(); stop {
			return perrs, stop
		}
		if len(*perrs) > v.MaxErrorItems {
			return v.NewTooManyErrors(perrs, prefix), true
		}
	}
	return perrs, false
}

func (v *State) Struct(ctx context.Context, ob interface{}, prefix string) (perrs *ErrorSet, stop bool) {
	switch ob := ob.(type) {
	case *Person:
		perrs, stop = v.person(ctx, ob, prefix+"Person#")
	case Person:
		perrs, stop = v.person(ctx, &ob, prefix+"Person#")
	default:
		return v.NewInternalError(fmt.Sprintf("internal error, unexpected type %T", ob)), true
	}

	if t, ok := ob.(interface {
		Validate(ctx context.Context, prefix string) *ErrorSet
	}); ok {
		if errs := t.Validate(ctx, prefix); errs != nil {
			for _, e := range *errs {
				if e, ok := e.(interface{ SetTranslator(t *Translator) }); ok {
					e.SetTranslator(v.T)
				}
			}
			*perrs = append(*perrs, *errs...)
		}
	}
	return perrs, stop
}

func (v *State) person(ctx context.Context, ob *Person, prefix string) (_ *ErrorSet, stop bool) {
	perrs := new(ErrorSet)

	// field: name, validation: required, type: string
	if path, value := prefix+"Name", ob.Name; value == zstring {
		*perrs = append(*perrs, &FieldError{Translator: v.T, Key: "name", Path: path, Tag: "required", Value: value})
	}

	// field: father, validation: struct
	if ob.Father != nil {
		if errs, stop := v.Struct(ctx, ob.Father, prefix+"Father."); errs != nil {
			*perrs = append(*perrs, *errs...)
			if stop {
				return perrs, stop
			}
		}
	}

	// field: children, validation: list
	if len(ob.Children) > 0 {
		prefix := prefix + "Children."
		for i, x := range ob.Children {
			errs, stop := v.Struct(ctx, &x, prefix+strconv.Itoa(i)+".")
			if errs != nil {
				*perrs = append(*perrs, *errs...)
			}
			if stop {
				return perrs, stop
			}
			if v.MaxErrorItems < len(*perrs) {
				return v.NewTooManyErrors(perrs, prefix), true
			}
		}
	}
	return perrs, false
}

var zstring string

type ErrorSet = runtime.ErrorSet
type Translator = runtime.Translator
type FieldError = runtime.FieldError
