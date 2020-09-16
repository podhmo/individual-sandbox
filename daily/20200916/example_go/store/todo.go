package store

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

type TodoStore struct {
	items []Todo
}

func (s *TodoStore) Add(item Todo) {
	s.items = append(s.items, item)
}
func (s *TodoStore) List() []Todo {
	return s.items
}

func NewTodoStore() *TodoStore {
	return &TodoStore{}
}
