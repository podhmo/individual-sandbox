package x

import (
	"context"
	"fmt"
	"m/runtime"
	v "m/validation"
)

type Person struct {
	Name   string  `json:"name"`
	Father *Person `json:"father`
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
}

func (v *Validator) Validate(ctx context.Context, ob interface{}) *ErrorSet {
	s := &State{
		T:             v.Translator,
		MaxErrorItems: -1,
	}
	return s.Struct(ctx, ob, "")
}

type State struct {
	T             *Translator
	MaxErrorItems int
}

func (v *State) NewInternalError(msg string) *ErrorSet {
	return &ErrorSet{
		&FieldError{Translator: v.T, Value: msg},
	}
}

func (v *State) Struct(ctx context.Context, ob interface{}, prefix string) *ErrorSet {
	var perrs *ErrorSet
	switch ob := ob.(type) {
	case *Person:
		if prefix == "" {
			prefix = "Person."
		}
		perrs = v.person(ctx, ob, prefix)
	default:
		return v.NewInternalError(fmt.Sprintf("internal error, unexpected type %T", ob))
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
	return perrs.NilOrError()
}

func (v *State) person(ctx context.Context, ob *Person, prefix string) *ErrorSet {
	perrs := new(ErrorSet)
	// field: name, validation: required
	if path, value := prefix+"Name", ob.Name; value == zstring {
		*perrs = append(*perrs, &FieldError{Translator: v.T, Key: "name", Path: path, Tag: "required", Value: value})
	}

	// field: father, validation: struct
	if ob.Father != nil {
		if errs := v.Struct(ctx, ob.Father, prefix+"Father."); errs != nil {
			*perrs = append(*perrs, *errs...)
		}
	}
	return perrs.NilOrError()
}

var zstring string

type ErrorSet = runtime.ErrorSet
type Translator = runtime.Translator
type FieldError = runtime.FieldError
