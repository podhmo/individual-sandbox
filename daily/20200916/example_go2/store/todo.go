package store

import (
	"context"
	"fmt"
	"m/store/entity"
)

var (
	ErrNotFound   = fmt.Errorf("not found")
	ErrUnexpected = fmt.Errorf("unexpected")
)

type TodoStore struct {
	items []entity.Todo
}

func (s *TodoStore) Add(ctx context.Context, item entity.Todo) error {
	s.items = append(s.items, item)
	return nil
}
func (s *TodoStore) List(ctx context.Context) ([]entity.Todo, error) {
	var newItems []entity.Todo
	for _, item := range s.items {
		if item.Done {
			continue
		}
		newItems = append(newItems, item)
	}
	return newItems, nil
}
func (s *TodoStore) Done(ctx context.Context, no int) ([]entity.Todo, error) {
	newItems := make([]entity.Todo, len(s.items))
	found := false
	for i := range s.items {
		item := s.items[i]
		if i == no {
			item.Done = true
			found = true
		}
		newItems[i] = item
	}
	if !found {
		return s.items, fmt.Errorf("done: %w", ErrNotFound)
	}
	s.items = newItems
	return newItems, nil
}

func NewTodoStore() *TodoStore {
	return &TodoStore{}
}
