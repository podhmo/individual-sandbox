package main

import (
	"errors"
	"fmt"
	"strings"

	"github.com/go-playground/validator"
	"github.com/morikuni/failure"
)

type Error struct { // todo: use failure.withCode{}
	Code   failure.StringCode
	Prefix string
	Err    error
}

func (e *Error) Error() string {
	return fmt.Sprintf("%s:\n\t%s", e.Prefix, strings.ReplaceAll(e.Err.Error(), "\n", "\n\t"))
}

// Unwrap is the methof for errors.Is() and errors.As()
func (e *Error) Unwrap() error {
	return e.Err
}

// As is the method for failure.CodeOf()
func (e *Error) As(x interface{}) bool {
	switch t := x.(type) {
	case *failure.Code:
		*t = e.Code
		return true
	case failure.Tracer:
		t.Push(e.Code)
		return true
	default:
		return false
	}
}

const BadInput failure.StringCode = "BadInput"

func NewValidationError(err error) *Error {
	return &Error{Prefix: "validation error", Err: err, Code: BadInput}
}

var validate = validator.New()

type Person struct {
	Name string `validate:"required"`
	Age  int    `validate:"required"`
}

func main() {
	var p Person
	if err := validate.Struct(p); err != nil {
		fmt.Println(NewValidationError(err))
	}

	fmt.Println("----------------------------------------")
	if err := validate.Struct(p); err != nil {
		var z validator.ValidationErrors
		fmt.Println("error as validate.Errors, ", errors.As(err, &z))

		err = NewValidationError(err)
		fmt.Println("wrapped error as validate.Errors, ", errors.As(err, &z))
		fmt.Println(failure.CodeOf(err))
		fmt.Printf("!! %+v\n", err)
	}
}
