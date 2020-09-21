package web

import "m/usecase"

func (s *Server) ResolveTodo(as AppSession) *usecase.Todo {
	return &usecase.Todo{StoreFactory: as}
}
