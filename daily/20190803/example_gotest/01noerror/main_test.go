package main

import (
	"fmt"
	"testing"

	"github.com/podhmo/noerror"
)

func Test(t *testing.T) {
	noerror.Should(t, fmt.Errorf("*hmm..*"), "hoi")
}
