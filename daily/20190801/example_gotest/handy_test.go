package main

import (
	"testing"

	"github.com/podhmo/handy"
)

func TestHandy(t *testing.T) {
	handy.Assert(t, handy.Equal(20).Actual(10+10))
	handy.Assert(t, handy.Equal(20).Actual(10))
	handy.Assert(t, handy.Equal(20).Actual(10).Describe("\nhello"))
}
