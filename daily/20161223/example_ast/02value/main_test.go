package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func pow(v Resolver, n int) Resolver {
	if n == 0 {
		return Int(1)
	}

	r := []Resolver{}
	for i := 1; i < n; i++ {
		r = append(r, v)
	}
	return Mul(v, r...)
}

func TestInt(t *testing.T) {
	type d struct {
		value    Resolver
		expected Atom
	}
	candidates := []d{
		{value: Int(1), expected: Int(1)},
		{value: Int(10), expected: Int(10)},
		{value: Add(Int(10), Int(1), Int(100)), expected: Int(111)},
		{value: Add(Add(Int(10), Int(1)), Int(100)), expected: Int(111)},
		{value: Add(Mul(Int(2), Int(2)), Int(2), Mul(Int(3), Int(3)), Int(3)), expected: Int(18)},
		{value: Add(Mul(Int(2), Int(2)), Int(2), Mul(Int(3), Int(3)), Int(3)), expected: Int(18)},
		{value: Add(
			pow(Int(2), 0), pow(Int(2), 1), pow(Int(2), 2),
			pow(Int(2), 3), pow(Int(2), 4), pow(Int(2), 5),
		),
			expected: Sub(pow(Int(2), 6), Int(1)).Resolve(),
		},
	}

	for i, c := range candidates {
		t.Run(fmt.Sprintf("check%d", i), func(t *testing.T) {
			actual := c.value.Resolve()
			assert.EqualValues(t, c.expected, actual)
		})
	}
}
func TestString(t *testing.T) {
	type d struct {
		value    Resolver
		expected Atom
	}
	candidates := []d{
		{value: Add(String("foo"), String("bar")), expected: String("foobar")},
		{value: Mul(String("foo"), Int(3)), expected: String("foofoofoo")},
		{value: Sub(Mul(String("foo"), Int(3)), Int(1), Int(1)), expected: String("ofoofoo")},
	}

	for i, c := range candidates {
		t.Run(fmt.Sprintf("check%d", i), func(t *testing.T) {
			actual := c.value.Resolve()
			assert.EqualValues(t, c.expected, actual)
		})
	}
}
