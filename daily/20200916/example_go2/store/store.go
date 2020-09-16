package store

type Store struct {
	Todo *TodoStore
}

func New() *Store {
	return &Store{
		Todo: NewTodoStore(),
	}
}
