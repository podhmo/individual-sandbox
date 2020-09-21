package usecase

import (
	"context"
	"m/store"
	"m/store/entity"

	"golang.org/x/xerrors"
)

type Todo struct {
	StoreFactory store.StoreFactory
}

// adding slack notification or something like this

func (u *Todo) ListTodo(ctx context.Context, r *[]entity.Todo) error {
	s, err := u.StoreFactory.NewStore()
	if err != nil {
		return xerrors.Errorf("new store: %w", err)
	}

	items, err := s.Todo.List(ctx)
	if err != nil {
		return xerrors.Errorf("list: %w", err)
	}
	*r = items
	return nil
}
func (u *Todo) AddTodo(ctx context.Context, r entity.Todo) error {
	s, err := u.StoreFactory.NewStore()
	if err != nil {
		return xerrors.Errorf("new store: %w", err)
	}

	if err := s.Todo.Add(ctx, r); err != nil {
		return xerrors.Errorf("add: %w", err)
	}
	return nil
}
func (u *Todo) DoneTodo(ctx context.Context, r *[]entity.Todo, no int) error {
	s, err := u.StoreFactory.NewStore()
	if err != nil {
		return xerrors.Errorf("new store: %w", err)
	}

	items, err := s.Todo.Done(ctx, no)
	if err != nil {
		return xerrors.Errorf("done: %w", err)
	}
	*r = items
	return nil
}
