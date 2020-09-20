package main

import (
	"bytes"
	"fmt"
	"reflect"
	"strings"
	"testing"
)

// todo: using interactor

func TestExecutor(t *testing.T) {
	newController := func(store []Todo) *Controller {
		var b bytes.Buffer
		return &Controller{
			Interactor: &Interactor{
				Store:  store,
				Writer: &b,
			},
		}
	}
	t.Run("add", func(t *testing.T) {
		c := newController(nil)
		err := c.AddC([]string{"hello\n"})
		if err != nil {
			t.Fatalf("unexpected: %+v", err)
		}

		got := c.Interactor.Store
		want := []Todo{
			{Title: "hello", Done: false},
		}
		if !reflect.DeepEqual(want, got) {
			t.Errorf("want:\n\t%#+v\nbut got:\n\t%#+v\n", want, got)
		}
	})

	t.Run("done", func(t *testing.T) {
		t.Run("ok", func(T *testing.T) {
			store := []Todo{
				{Title: "hello", Done: false},
			}
			c := newController(store)

			err := c.DoneC([]string{"0"})
			if err != nil {
				t.Fatalf("unexpected: %+v", err)
			}

			got := c.Interactor.Store
			want := []Todo{
				{Title: "hello", Done: true},
			}
			if !reflect.DeepEqual(want, got) {
				t.Errorf("want:\n\t%#+v\nbut got:\n\t%#+v\n", want, got)
			}
		})

		t.Run("invalid input", func(t *testing.T) {
			c := newController(nil)

			err := c.DoneC([]string{"xxx0"})
			if err == nil {
				t.Fatalf("error expected but nil")
			}
		})

		t.Run("not found", func(t *testing.T) {
			c := newController(nil)

			err := c.DoneC([]string{"0"})
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
				ctr := newController(c.store)
				ctr.Writer = &b

				err := ctr.ListC(nil)
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
