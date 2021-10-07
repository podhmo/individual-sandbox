package main

import (
	"errors"
	"fmt"

	"github.com/go-playground/validator"
	"github.com/morikuni/failure"
)

const BadInput failure.StringCode = "BadInput"

var validate = validator.New()

type Person struct {
	Name string `validate:"required"`
	Age  int    `validate:"required"`
}

func Extract(err error) error {
	if err == nil {
		return nil
	}
	return failure.Custom(failure.Custom(err), failure.WithFormatter(), failure.WithCallStackSkip(1))
	// i := failure.NewIterator(err)
	// for i.Next() {
	// 	if v, ok := i.Error().(interface{ Unexpected() bool }); ok && v.Unexpected() {
	// 		return i.Error()
	// 	}

	// 	var c failure.Code
	// 	if i.As(&c) {
	// 		return i.Error()
	// 	}
	// }
	// return err
}

func main() {
	var p Person
	if err := validate.Struct(p); err != nil {
		fmt.Println(failure.Translate(err, BadInput))
	}

	fmt.Println("----------------------------------------")
	if err := validate.Struct(p); err != nil {
		var z validator.ValidationErrors
		fmt.Println("error as validate.Errors, ", errors.As(err, &z))

		err = failure.Translate(err, BadInput)
		fmt.Println("wrapped error as validate.Errors, ", errors.As(err, &z))
		fmt.Println(err)
		fmt.Println(failure.CodeOf(err))
		fmt.Printf("!!%+v\n", err)
	}

	fmt.Println("----------------------------------------")
	if err := validate.Struct(p); err != nil {
		var z validator.ValidationErrors
		fmt.Println("error as validate.Errors, ", errors.As(err, &z))

		err = fmt.Errorf("xxxx %w", fmt.Errorf("WRAPPING, %w", failure.Translate(err, BadInput)))
		fmt.Println("wrapped error as validate.Errors, ", errors.As(err, &z))
		fmt.Println(err)
		fmt.Println(failure.CodeOf(err))
		fmt.Printf("!!%+v\n", err)
	}

	fmt.Println("----------------------------------------")
	if err := validate.Struct(p); err != nil {
		var z validator.ValidationErrors
		fmt.Println("error as validate.Errors, ", errors.As(err, &z))

		err = Extract(fmt.Errorf("xxxx %w", fmt.Errorf("WRAPPING, %w", failure.Translate(err, BadInput))))
		fmt.Println("wrapped error as validate.Errors, ", errors.As(err, &z))
		fmt.Println(err)
		fmt.Println(failure.CodeOf(err))
		fmt.Println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") // fmt.Errorf()の対応が無理
		fmt.Printf("!!%+v\n", err)
	}
}
