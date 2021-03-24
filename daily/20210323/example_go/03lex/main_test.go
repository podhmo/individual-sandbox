package main

import (
	"reflect"
	"testing"
)

func Test(t *testing.T) {
	cases := []struct {
		input  string
		output [][2]string
	}{
		{
			input:  ``,
			output: nil,
		},
		{
			input:  `@notzero`,
			output: [][2]string{{"@notzero", ""}},
		},
		{
			input:  `@pattern=[A-Z].*`,
			output: [][2]string{{"@pattern", "[A-Z].*"}},
		},
		{
			input:  `@pattern=[A-Z].*,@notzero`,
			output: [][2]string{{"@pattern", "[A-Z].*"}, {"@notzero", ""}},
		},
		{
			input:  `@notzero,@pattern=[A-Z].*`,
			output: [][2]string{{"@notzero", ""}, {"@pattern", "[A-Z].*"}},
		},
		{
			input:  ` @pattern=[A-Z].* , @notzero `, // with white space
			output: [][2]string{{"@pattern", "[A-Z].*"}, {"@notzero", ""}},
		},
		{
			input:  `@min-length=1,@pattern=\d+$,@unique`,
			output: [][2]string{{"@min-length", "1"}, {"@pattern", "\\d+$"}, {"@unique", ""}},
		},
		{
			input:  `@min-length=1,@pattern=\d+@xxx=+=$,@unique`, // bug
			output: [][2]string{{"@min-length", "1"}, {"@pattern", "\\d+"}, {"@xxx", "+=$"}, {"@unique", ""}},
		},
		{
			input:  `@min-length=1,@pattern=\d+[@]xxx=+=$,@unique`, // work-around
			output: [][2]string{{"@min-length", "1"}, {"@pattern", "\\d+[@]xxx=+=$"}, {"@unique", ""}},
		},
		{
			input:  `@@positive=,@max-length=99`,
			output: [][2]string{{"@@positive", ""}, {"@max-length", "99"}},
		},
		{
			input:  `@pattern=(\S+,\s*\S+),@notzero`,
			output: [][2]string{{"@pattern", "(\\S+,\\s*\\S+)"}, {"@notzero", ""}},
		},
		{
			input: `@maximum=50,exclusiveMaximum=true,@notzero,@minimum=0,exclusiveMinimum=false`,
			output: [][2]string{
				{"@maximum", "50,exclusiveMaximum=true"}, {"@notzero", ""}, {"@minimum", "0,exclusiveMinimum=false"},
			},
		},
	}
	for _, c := range cases {
		t.Run(c.input, func(t *testing.T) {
			got := Lex(c.input)
			if !reflect.DeepEqual(c.output, got) {
				t.Errorf("want:\n\t%q\nbut got:\n\t%q", c.output, got)
			}
		})
	}
}
