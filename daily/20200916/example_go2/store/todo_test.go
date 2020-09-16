package store

import (
	"context"
	"reflect"
	"testing"
)

func TestTodo(t *testing.T) {
	t.Run("empty list", func(t *testing.T) {
		var want []*Todo

		s := NewTodoStore()
		ctx := context.Background()
		var got []*Todo
		if err := s.List(ctx, &got); err != nil {
			t.Fatalf("! %+v", err)
		}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("want %#+v, but got %#+v", want, got)
		}
	})

	t.Run("add", func(t *testing.T) {
		todo := &Todo{
			Title: "Go to bed",
		}
		want := []*Todo{todo}

		s := NewTodoStore()
		ctx := context.Background()
		if err := s.Add(ctx, todo); err != nil {
			t.Fatalf("! %+v", err)
		}
		var got []*Todo
		if err := s.List(ctx, &got); err != nil {
			t.Fatalf("! %+v", err)
		}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("want %#+v, but got %#+v", want, got)
		}
	})
}
