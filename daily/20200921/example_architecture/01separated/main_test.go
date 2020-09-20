package main

import (
	"bytes"
	"fmt"
	"reflect"
	"strings"
	"testing"
)

func TestExecutor(t *testing.T) {
	t.Run("add", func(t *testing.T) {
		var b bytes.Buffer
		ex := &Executor{Writer: &b}

		err := ex.Add([]string{"hello\n"})
		if err != nil {
			t.Fatalf("unexpected: %+v", err)
		}

		got := ex.Store
		want := []Todo{
			{Title: "hello", Done: false},
		}
		if !reflect.DeepEqual(want, got) {
			t.Errorf("want:\n\t%#+v\nbut got:\n\t%#+v\n", want, got)
		}
	})

	t.Run("done", func(t *testing.T) {
		t.Run("ok", func(T *testing.T) {
			var b bytes.Buffer
			store := []Todo{
				{Title: "hello", Done: false},
			}
			ex := &Executor{Writer: &b, Store: store}

			err := ex.Done([]string{"0"})
			if err != nil {
				t.Fatalf("unexpected: %+v", err)
			}

			got := ex.Store
			want := []Todo{
				{Title: "hello", Done: true},
			}
			if !reflect.DeepEqual(want, got) {
				t.Errorf("want:\n\t%#+v\nbut got:\n\t%#+v\n", want, got)
			}
		})

		t.Run("invalid input", func(t *testing.T) {
			var b bytes.Buffer
			ex := &Executor{Writer: &b}

			err := ex.Done([]string{"xxx0"})
			if err == nil {
				t.Fatalf("error expected but nil")
			}
		})

		t.Run("not found", func(t *testing.T) {
			var b bytes.Buffer
			ex := &Executor{Writer: &b}

			err := ex.Done([]string{"0"})
			if err == nil {
				t.Fatalf("error expected but nil")
			}
		})
	})

	t.Run("list", func(t *testing.T) {
		cases := []struct {
			store []Todo
			want  string
		}{
			{
				store: []Todo{},
				want:  ``,
			},
			{
				store: []Todo{
					{Title: "hello world"},
				},
				want: `
00: hello world
`,
			},
			{
				store: []Todo{
					{Title: "hello world", Done: true},
				},
				want: ``,
			},
		}
		for i, c := range cases {
			c := c
			t.Run(fmt.Sprintf(":%d", i), func(t *testing.T) {
				var b bytes.Buffer
				ex := &Executor{
					Writer: &b,
					Store:  c.store,
				}
				err := ex.List(nil)
				if err != nil {
					t.Fatalf("unexpected: %+v", err)
				}

				got := strings.TrimSpace(b.String())
				want := strings.TrimSpace(c.want)
				if want != got {
					t.Errorf("want:\n\t%q\nbut got:\n\t%q\n", want, got)
				}
			})
		}
	})
}
