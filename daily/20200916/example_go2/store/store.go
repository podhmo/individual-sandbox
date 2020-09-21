package store

type StoreFactory interface {
	NewStore() (*Store, error)
}

type Store struct {
	Todo *TodoStore
}

func New() *Store {
	return &Store{
		Todo: NewTodoStore(),
	}
}
