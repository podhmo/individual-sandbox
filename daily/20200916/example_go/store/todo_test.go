package store

import (
	"reflect"
	"testing"
)

func TestTodo(t *testing.T) {
	t.Run("empty list", func(t *testing.T) {
		s := NewTodoStore()
		got := s.List()
		var want []Todo
		if !reflect.DeepEqual(got, want) {
			t.Errorf("want %#+v, but got %#+v", want, got)
		}
	})
}
