package main

import (
	"fmt"

	multierror "github.com/hashicorp/go-multierror"
	"github.com/pkg/errors"
)

func f() error {
	var merr *multierror.Error
	return merr.ErrorOrNil()
}

func g() error {
	var merr *multierror.Error
	merr = multierror.Append(merr, errors.New("hmm"))
	return merr.ErrorOrNil()
}

func h() error {
	var merr *multierror.Error
	merr = multierror.Append(merr, errors.New("hmm"))
	merr = multierror.Append(merr, errors.New("hmm"))
	merr = multierror.Append(merr, errors.New("hmm"))
	merr = multierror.Append(merr, errors.New("hmm"))
	merr = multierror.Append(merr, errors.WithMessage(errors.New("hmm"), "last"))
	return merr.ErrorOrNil()
}

func main() {
	if err := f(); err != nil {
		fmt.Println(err)
	}
	if err := g(); err != nil {
		fmt.Println(err)
	}
	if err := h(); err != nil {
		fmt.Println(err)
	}
}
