package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestMatch(t *testing.T) {
	type d struct {
		fn       MatchFunc
		value    string
		expected bool
	}

	candidates := []d{
		{fn: Contains("foo"), value: "foobar", expected: true},
		{fn: Startswith("foo"), value: "foobar", expected: true},
		{fn: Endswith("foo"), value: "foobar", expected: false},
		{fn: Or(Contains("foo"), Contains("boo")), value: "foobar", expected: true},
		{fn: And(Contains("foo"), Contains("boo")), value: "foobar", expected: false},
		{fn: Or(And(Startswith("fo"), Endswith("ar")), Contains("@")), value: "foobar", expected: true},
		{fn: Regex("^f"), value: "foobar", expected: true},
		{fn: And(Regex("^f"), Regex("r$")), value: "foobar", expected: true},
	}

	for i, c := range candidates {
		t.Run(fmt.Sprintf("check:%d", i), func(t *testing.T) {
			actual := c.fn(c.value)
			assert.Exactly(t, c.expected, actual)
		})
	}
}
