package main

import (
	"encoding/json"
	"fmt"
	"io"

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
	return &formatter{error: err}
}

func WithFormatter() failure.Wrapper {
	return failure.WrapperFunc(func(err error) error {
		return &formatter{err}
	})
}

type formatter struct {
	error
}

func (f *formatter) Unwrap() error {
	return f.error
}

func (*formatter) IsFormatter() {}

func (f *formatter) Format(s fmt.State, verb rune) {
	if verb != 'v' { // %s
		io.WriteString(s, f.Error())
		return
	}

	if s.Flag('#') { // %#v
		type formatter struct {
			error
		}
		fmt.Fprintf(s, "%#v", &formatter{f.error})
		return
	}

	if !s.Flag('+') { // %v
		io.WriteString(s, f.Error())
		return
	}

	// %+v
	type formatter interface {
		IsFormatter()
	}

	enc := json.NewEncoder(s)
	enc.SetIndent("", "  ")
	var textlist []string

	i := failure.NewIterator(f.error)
	for i.Next() {
		err := i.Error()
		if _, ok := err.(formatter); ok {
			continue
		}
		var (
			cs   failure.CallStack
			ctx  failure.Context
			msg  failure.Message
			code failure.Code
		)
		switch {
		case i.As(&cs):
			textlist = append(textlist, fmt.Sprintf("%+v\n", cs.HeadFrame()))
		case i.As(&ctx):
			for k, v := range ctx {
				textlist = append(textlist, fmt.Sprintf("    %s = %s\n", k, v))
			}
		case i.As(&msg):
			textlist = append(textlist, fmt.Sprintf("    message(%q)\n", msg))
		case i.As(&code):
			textlist = append(textlist, fmt.Sprintf("    code(%s)\n", code.ErrorCode()))
		default:
			textlist = append(textlist, fmt.Sprintf("    %T(%q)\n", err, err.Error()))
		}
	}

	var callstack []string
	if cs, ok := failure.CallStackOf(f); ok {
		for _, f := range cs.Frames() {
			callstack = append(callstack, fmt.Sprintf("%+v\n", f))
		}
	}
	if err := enc.Encode(map[string]interface{}{"text": textlist, "callstack": callstack}); err != nil {
		fmt.Fprintf(s, "%v -- error %s", f, err)
	}
}
func main() {
	var p Person
	fmt.Println("----------------------------------------")
	if err := validate.Struct(p); err != nil {
		err = fmt.Errorf("xxxx %w", fmt.Errorf("WRAPPING, %w", failure.Translate(err, BadInput)))
		err = Extract(err)
		fmt.Println(err)
		fmt.Println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") // fmt.Errorf()の対応が厳しい
		fmt.Printf("!!%+v\n", err)
		fmt.Println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") // fmt.Errorf()の対応が厳しい

		err = failure.New(BadInput)
		err = Extract(err)
		fmt.Printf("!!%+v\n", err)
	}
}
