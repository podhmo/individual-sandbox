package store

import "context"

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

type TodoStore struct {
	items []*Todo
}

func (s *TodoStore) Add(ctx context.Context, item *Todo) error {
	s.items = append(s.items, item)
	return nil
}
func (s *TodoStore) List(ctx context.Context, r *[]*Todo) error {
	*r = s.items
	return nil
}

func NewTodoStore() *TodoStore {
	return &TodoStore{}
}
