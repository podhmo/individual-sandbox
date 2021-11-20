package main

import (
	"fmt"
	"strings"
)

type Person struct {
	Name string
}

func main() {
	p := Person{}
	fmt.Println(ValidatePerson(&p))
	fmt.Println(ValidateString(""))
	fmt.Println(ValidateString("foo"))
}

func ValidatePerson(ob *Person) error {
	perrs := new(ErrorSet)
	if path, value := "Person.Name", ob.Name; value == zstring {
		*perrs = append(*perrs, &FieldError{Path: path, Message: "not blank", Tag: "required", Value: value})
	}
	return perrs.NilOrError()
}

// func stringRequired(errs *ErrorSet, path string, value string) *FieldError {
// 	// todo: getFieldName
// 	if value == zstring {
// 		*errs = append(*errs, &FieldError{Path: path, Message: "not blank", Tag: "required", Value: value})
// 	}
// 	return nil
// }

func ValidateString(value string) error {
	perrs := new(ErrorSet)
	if path, value := "", value; value == zstring {
		// todo: getFieldName
		*perrs = append(*perrs, &FieldError{Path: path, Message: "not blank", Tag: "required", Value: value})
	}
	return perrs.NilOrError()
}

var zstring string

type ErrorSet []error

func (es ErrorSet) NilOrError() error {
	if len(es) > 0 {
		return es
	}
	return nil
}

func (es ErrorSet) Error() string {
	r := make([]string, len(es))
	for i, e := range es {
		r[i] = e.Error()
	}
	return strings.Join(r, "\n")
}

type FieldError struct {
	Path    string
	Message string
	Tag     string
	Value   interface{}
}

func (e *FieldError) Error() string {
	return fmt.Sprintf("%s: %s", e.Path, e.Message)
}
