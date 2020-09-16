package store

import (
	"context"
	"m/store/entity"
	"reflect"
	"testing"
)

func TestTodo(t *testing.T) {
	t.Run("empty list", func(t *testing.T) {
		var want []*entity.Todo

		s := NewTodoStore()
		ctx := context.Background()
		var got []*entity.Todo
		if err := s.List(ctx, &got); err != nil {
			t.Fatalf("! %+v", err)
		}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("want %#+v, but got %#+v", want, got)
		}
	})

	t.Run("add", func(t *testing.T) {
		todo := &entity.Todo{
			Title: "Go to bed",
		}
		want := []*entity.Todo{todo}

		s := NewTodoStore()
		ctx := context.Background()
		if err := s.Add(ctx, todo); err != nil {
			t.Fatalf("! %+v", err)
		}
		var got []*entity.Todo
		if err := s.List(ctx, &got); err != nil {
			t.Fatalf("! %+v", err)
		}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("want %#+v, but got %#+v", want, got)
		}
	})
}
