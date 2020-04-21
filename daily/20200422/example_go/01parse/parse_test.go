package parse

import (
	"flag"
	"fmt"
	"strings"
	"testing"
)

func TestParse(t *testing.T) {
	cases := [][]string{
		{"-name", "foo"},
		{"--name", "foo"},
		{"-name=foo"},
		{"--name=foo"},
		{"-n", "foo"},
		{"--n", "foo"},
		{"-n=foo"},
		{"--n=foo"},
	}

	for _, args := range cases {
		args := args
		t.Run(fmt.Sprintf("%q", args), func(t *testing.T) {
			opt := &Option{}
			cmd := New(opt)

			err := cmd.Parse(args)
			if err != nil {
				t.Fatalf("expect nil, but error %+v", err)
			}

			if opt.Name != "foo" {
				t.Errorf("expected %q, but %q", "foo", opt.Name)
			}
		})
	}
}

func TestParseDefault(t *testing.T) {
	args := []string{}
	opt := &Option{}
	cmd := New(opt)

	err := cmd.Parse(args)
	if err != nil {
		t.Fatalf("expect nil, but error %+v", err)
	}

	if opt.Name != "" {
		t.Errorf("expected %q, but %q", "", opt.Name)
	}
}

func TestHelp(t *testing.T) {
	cases := [][]string{
		{"-h"},
		{"--help"},
	}

	for _, args := range cases {
		args := args
		t.Run(fmt.Sprintf("%q", args), func(t *testing.T) {
			opt := &Option{}
			cmd := New(opt)

			err := cmd.Parse(args)
			if err == nil {
				t.Fatal("expect error, but nil")
			}
			if err != flag.ErrHelp {
				t.Errorf("must be %s", err)
			}

			output := cmd.Output().(*strings.Builder).String()
			expected := `Usage of app:
  -n string
    	<name> (shorthand)
  -name string
    	<name>
`
			if expected != output {
				t.Errorf(
					"expected %q, but %q",
					strings.TrimSpace(output),
					strings.TrimSpace(expected),
				)
			}
		})
	}
}
