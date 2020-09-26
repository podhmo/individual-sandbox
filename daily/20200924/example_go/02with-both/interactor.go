package main

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

type Interactor struct {
	Store []Todo
}

func (ir *Interactor) List() ([]Todo, error) {
	return ir.Store, nil
}

func (ir *Interactor) Add(todo Todo) (Todo, error) {
	ir.Store = append(ir.Store, todo)
	return todo, nil
}

var (
	store = []Todo{
		{Title: "hello"},
		{Title: "bye bye"},
	}
)

func GetInteractor() *Interactor{
	return &Interactor{Store: store}
}
