package main

import (
	"fmt"
	"io"

	"github.com/go-playground/validator"
)

type Params struct{}

type Accessor interface {
	New() Params
	Execute(io.Writer, Params) error
}

type Manager struct {
	validate *validator.Validate
}

func main() {

	m := Manager{}
	xxx := m.LookupXXX()
	params := xxx.New()
	params.Items = []string{"foo", "bar"}
	fmt.Println(xxx.Execute(io.Writer, params))
}
