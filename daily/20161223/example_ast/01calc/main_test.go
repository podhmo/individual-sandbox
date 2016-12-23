package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func pow(v Resolver, n int) Resolver {
	if n == 0 {
		return Value(1)
	}

	r := []Resolver{}
	for i := 0; i < n; i++ {
		r = append(r, v)
	}
	return Mul(r...)
}

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
		{value: Add(Mul(Value(2), Value(2)), Value(2), Mul(Value(3), Value(3)), Value(3)), expected: 18},
		{value: Add(Mul(Value(2), Value(2)), Value(2), Mul(Value(3), Value(3)), Value(3)), expected: 18},
		{value: Add(
            pow(Value(2), 0), pow(Value(2), 1), pow(Value(2), 2),
			pow(Value(2), 3), pow(Value(2), 4), pow(Value(2), 5),
		),
			expected: pow(Value(2), 6).Resolve() - 1,
		},
	}

	for i, c := range candidates {
		t.Run(fmt.Sprintf("check%d", i), func(t *testing.T) {
			actual := c.value.Resolve()
			assert.Exactly(t, c.expected, actual)
		})
	}
}
