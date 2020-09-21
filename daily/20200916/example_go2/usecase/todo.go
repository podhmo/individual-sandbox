package usecase

import (
	"context"
	"m/store"
	"m/store/entity"

	"golang.org/x/xerrors"
)

type Todo struct {
	Store *store.Store
}

// adding slack notification or something like this

func (u *Todo) ListTodo(ctx context.Context, r *[]entity.Todo) error {
	items, err := u.Store.Todo.List(ctx)
	if err != nil {
		return xerrors.Errorf("list: %w", err)
	}
	if len(items) == 0 {
		items = []entity.Todo{}
	}
	*r = items
	return nil
}
func (u *Todo) AddTodo(ctx context.Context, r entity.Todo) error {
	if err := u.Store.Todo.Add(ctx, r); err != nil {
		return xerrors.Errorf("add: %w", err)
	}
	return nil
}
func (u *Todo) DoneTodo(ctx context.Context, r *[]entity.Todo, no int) error {
	items, err := u.Store.Todo.Done(ctx, no)
	if err != nil {
		return xerrors.Errorf("done: %w", err)
	}
	if len(items) == 0 {
		items = []entity.Todo{}
	}
	*r = items
	return nil
}
