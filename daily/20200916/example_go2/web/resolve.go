package web

import (
	"m/usecase"

	"golang.org/x/xerrors"
)

func (s *Server) ResolveTodo(as AppSession) (*usecase.Todo, error) {
	store, err := as.NewStore()
	if err != nil {
		return nil, xerrors.Errorf("new store: %w", err)
	}
	return &usecase.Todo{Store: store}, nil
}
