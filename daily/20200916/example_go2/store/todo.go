package store

import (
	"context"
	"errors"
	"m/store/entity"
)

type TodoStore struct {
	items []*entity.Todo
}

func (s *TodoStore) Add(ctx context.Context, item *entity.Todo) error {
	return errors.New("hmm")
	// s.items = append(s.items, item)
}
func (s *TodoStore) List(ctx context.Context, r *[]*entity.Todo) error {
	*r = s.items
	return nil
}

func NewTodoStore() *TodoStore {
	return &TodoStore{}
}
