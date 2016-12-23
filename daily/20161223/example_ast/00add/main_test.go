package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestValue(t *testing.T) {
	type d struct {
		value    Resolver
		expected int
	}
	candidates := []d{
		{value: Value(1), expected: 1},
		{value: Value(10), expected: 10},
		{value: Add(Value(10), Value(1), Value(100)), expected: 111},
		{value: Add(Add(Value(10), Value(1)), Value(100)), expected: 111},
	}

	for i, c := range candidates {
		t.Run(fmt.Sprintf("check%d", i), func(t *testing.T) {
			actual := c.value.Resolve()
			assert.Exactly(t, c.expected, actual)
		})
	}
}
