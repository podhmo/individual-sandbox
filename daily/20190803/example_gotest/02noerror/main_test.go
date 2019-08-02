package main

import (
	"testing"

	"github.com/pkg/errors"
	"github.com/podhmo/noerror"
)

func f() error {
	return g()
}

func g() error {
	return errors.Errorf("hmm")
}

func Test(t *testing.T) {
	noerror.Should(t, f())
}
