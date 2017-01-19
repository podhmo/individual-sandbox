package main

import (
	"errors"
	"fmt"

	multierror "github.com/hashicorp/go-multierror"
)

func g() ([]bool, error) {
	var r []bool
	ok0 := true
	ok1 := false
	ok2 := true
	var errs *multierror.Error
	if !ok0 {
		multierror.Append(errs, errors.New("ng0"))
	}
	r = append(r, ok0)
	if !ok1 {
		multierror.Append(errs, errors.New("ng1"))
	}
	r = append(r, ok1)
	if !ok2 {
		multierror.Append(errs, errors.New("ng2"))
	}
	r = append(r, ok2)
	return r, errs.ErrorOrNil()
}

func main() {
	r, err := g()
	fmt.Println(r, err)
    // [true false true] <nil>
}
