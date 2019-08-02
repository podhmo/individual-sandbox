package main

import (
	"fmt"
	"testing"

	"github.com/podhmo/noerror"
)

func Test(t *testing.T) {
	noerror.Should(t, noerror.Equal(10).Actual(1+1))
	noerror.Should(t, noerror.Equal(10).Actual(1+1).Describe("1+1"))

	Count := func() (int, error) { return 0, fmt.Errorf("*ERROR*") }
	noerror.Should(t, noerror.Equal(0).ActualWithError(Count()))
}
