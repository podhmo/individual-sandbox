package main

import (
	"errors"
	"fmt"
	"strings"

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

	i := failure.NewIterator(err)
	for i.Next() {
		inner := i.Error()
		if v, ok := inner.(interface {
			IsFormatter()
			Unwrap() error
		}); ok {
			if err == inner {
				return err
			}
			prefix := strings.TrimSpace(strings.ReplaceAll(err.Error(), inner.Error(), "")) // todo: lazy
			return failure.Custom(
				failure.Custom(v.Unwrap(), failure.Message(prefix)),
				failure.WithFormatter(), failure.WithCallStackSkip(1),
			)
		}
	}
	return err
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

		err = fmt.Errorf("xxxx %w", fmt.Errorf("WRAPPING, %w", failure.Translate(err, BadInput)))
		err = Extract(err)
		fmt.Println("wrapped error as validate.Errors, ", errors.As(err, &z))
		fmt.Println(err)
		fmt.Println(failure.CodeOf(err))
		fmt.Println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") // fmt.Errorf()の対応が厳しい
		fmt.Printf("!!%+v\n", err)
		fmt.Println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") // fmt.Errorf()の対応が厳しい

		err = failure.New(BadInput)
		err = Extract(err)
		fmt.Printf("!!%+v\n", err)
	}
}
