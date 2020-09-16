package store

import (
	"context"
	"m/store/entity"
)

type TodoStore struct {
	items []*entity.Todo
}

func (s *TodoStore) Add(ctx context.Context, item *entity.Todo) error {
	s.items = append(s.items, item)
	return nil
}
func (s *TodoStore) List(ctx context.Context, r *[]*entity.Todo) error {
	*r = s.items
	return nil
}

func NewTodoStore() *TodoStore {
	return &TodoStore{}
}
