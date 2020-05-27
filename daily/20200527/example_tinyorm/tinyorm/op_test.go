package tinyorm

import (
	"fmt"
	"reflect"
	"testing"
)

func TestOpFormat(t *testing.T) {
	cases := []struct {
		input interface{}
		want  string
	}{
		{
			input: And("x", "y"),
			want:  "(x AND y)",
		},
		{
			input: And("x", "y", "z"),
			want:  "(x AND y AND z)",
		},
		{
			input: And("x", Or("y", "z")),
			want:  "(x AND (y OR z))",
		},
		{
			input: And(Or("x", "y"), "z"),
			want:  "((x OR y) AND z)",
		},
		{
			input: And(Or("x", Not("y")), "z"),
			want:  "((x OR (NOT y)) AND z)",
		},
		{
			input: And(Or("x", "y"), Not(Not("z"))),
			want:  "((x OR y) AND (NOT (NOT z)))",
		},
	}

	for i, c := range cases {
		c := c
		t.Run(fmt.Sprintf("case:%d", i), func(t *testing.T) {
			got := fmt.Sprintf("%v", c.input)
			if c.want != got {
				t.Errorf("\nwant\n\t%v\nbut\n\t%s", c.want, got)
			}
		})
	}
}

func TestOpValues(t *testing.T) {
	cases := []struct {
		input interface{}
		want  []interface{}
	}{
		{
			input: "x",
			want:  []interface{}{"x"},
		},
		{
			input: And("x", "y"),
			want:  []interface{}{"x", "y"},
		},
		{
			input: And("x", "y", "z"),
			want:  []interface{}{"x", "y", "z"},
		},
		{
			input: And("x", Or("y", "z")),
			want:  []interface{}{"x", "y", "z"},
		},
		{
			input: And(Or("x", "y"), "z"),
			want:  []interface{}{"x", "y", "z"},
		},
		{
			input: And(Or("x", Not("y")), "z"),
			want:  []interface{}{"x", "y", "z"},
		},
		{
			input: And(Or("x", "y"), Not(Not("z"))),
			want:  []interface{}{"x", "y", "z"},
		},
	}

	for i, c := range cases {
		c := c
		t.Run(fmt.Sprintf("case:%d", i), func(t *testing.T) {
			got := Values(c.input)
			if !reflect.DeepEqual(c.want, got) {
				t.Errorf("\nwant\n\t%v\nbut\n\t%s", c.want, got)
			}
		})
	}
}
