package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"

	"github.com/go-playground/validator/v10"
)

type Validator struct {
	v *validator.Validate
}

func (v *Validator) Struct(s interface{}) error {
	err := v.v.Struct(s)
	if err == nil {
		return nil
	}
	switch err := err.(type) {
	case validator.ValidationErrors:
		return ValidationError(err)
	case *validator.InvalidValidationError:
		return err
	default:
		return err
	}
}

func New() *Validator {
	return &Validator{
		v: validator.New(),
	}
}

type ValidationError validator.ValidationErrors

func (err ValidationError) Error() string {
	return validator.ValidationErrors(err).Error()
}

func (err ValidationError) MarshalJSON() ([]byte, error) {
	b := bytes.NewBuffer(nil)
	fields := validator.ValidationErrors(err)

	n := len(fields) - 1
	b.WriteString(`[`)
	for i, field := range fields {
		fmt.Fprintf(b, `{%q: "failed on the '%s' tag"}`, field.Namespace(), field.Tag())
		if i < n {
			b.WriteString(`,`)
		}
	}
	b.WriteString(`]`)
	return b.Bytes(), nil
}

type S struct {
	Name string `validate:"required"`
	Age  int    `validate:"required"`
}

func main() {
	validate := New()
	err := validate.Struct(S{})
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	fmt.Println("!", enc.Encode(err))
}
