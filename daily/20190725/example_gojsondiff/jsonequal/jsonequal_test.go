package jsonequal

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestEqual(t *testing.T) {
	type C struct {
		msg    string
		left   func() (interface{}, []byte, error)
		right  func() (interface{}, []byte, error)
		assert func(*testing.T, error)
	}

	ok := func(t *testing.T, err error) {
		assert.NoError(t, err)
	}
	ng := func(t *testing.T, err error) {
		assert.Error(t, err)
	}

	cases := []C{
		{
			msg:    "map==map",
			left:   From(map[string]int{"foo": 1}),
			right:  From(map[string]int{"foo": 1}),
			assert: ok,
		},
		{
			msg:    "*map==*map",
			left:   From(&map[string]int{"foo": 1}),
			right:  From(&map[string]int{"foo": 1}),
			assert: ok,
		},
		(func() C {
			v := map[string]int{"foo": 1}
			return C{
				msg:    "*map==*map and map==map",
				left:   From(&v),
				right:  From(&v),
				assert: ok,
			}
		})(),
		(func() C {
			v := map[string]int{"foo": 1}
			return C{
				msg:    "*map==map",
				left:   From(&v),
				right:  From(v),
				assert: ok,
			}
		})(),
		{
			msg:    "map!=map",
			left:   From(map[string]int{"foo": 1}),
			right:  From(map[string]int{"foo": 2}),
			assert: ng,
		},
		{
			msg:  "struct==map",
			left: From(map[string]int{"foo": 1}),
			right: From(struct {
				Foo int `json:"foo"`
			}{Foo: 1}),
			assert: ok,
		},
		{
			msg:  "struct!=map",
			left: From(map[string]int{"foo": 1}),
			right: From(struct {
				Foo int `json:"foo"`
			}{Foo: 2}),
			assert: ng,
		},
		{
			msg:  "[]byte==struct",
			left: FromBytes([]byte(`{"foo": 1}`)),
			right: From(struct {
				Foo int `json:"foo"`
			}{Foo: 1}),
			assert: ok,
		},
		{
			msg:  "[]byte!=struct",
			left: FromBytes([]byte(`{"foo": 1}`)),
			right: From(struct {
				Foo int `json:"foo"`
			}{Foo: 2}),
			assert: ng,
		},
		{
			msg:    "[]byte==reader",
			left:   FromBytes([]byte(`{"foo": 1}`)),
			right:  FromReader(bytes.NewBufferString(`{"foo": 1}`)),
			assert: ok,
		},
		{
			msg:    "[]byte!=reader",
			left:   FromBytes([]byte(`{"foo": 1}`)),
			right:  FromReader(bytes.NewBufferString(`{"boo": 1}`)),
			assert: ng,
		},
	}

	for _, c := range cases {
		c := c
		t.Run(c.msg, func(t *testing.T) {
			got := Equal(c.left, c.right)
			c.assert(t, got)
		})
	}
}
