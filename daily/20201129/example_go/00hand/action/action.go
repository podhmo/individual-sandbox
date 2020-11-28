package action

import (
	"context"
	"fmt"
	"m/00hand/apperror"
)

type User struct {
	Name string `json:"name"`
}

func Hello(ctx context.Context, user User, short *bool) (string, error) {
	name := user.Name
	if short != nil && *short {
		return fmt.Sprintf("Hi %s", name), nil
	}
	return fmt.Sprintf("Hello %s", name), nil
}

func IsEven(ctx context.Context, v int) (string, error) {
	if v%2 == 0 {
		return "ok", nil
	}
	return "ng", apperror.New(fmt.Errorf("not even %+v", v), 400)
}

// ---

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

type TodoService struct {
	Todos []Todo
}

func (s *TodoService) Add(ctx context.Context, todo Todo) (int, error) {
	s.Todos = append(s.Todos, todo)
	return len(s.Todos), nil
}

func (s *TodoService) List(ctx context.Context) ([]Todo, error) {
	return s.Todos, nil
}

// -- component
type Greeter struct{}

func (g *Greeter) Greet(name string) string {
	return "Hello, " + name
}
func Greet(ctx context.Context, getGreeter func() *Greeter, name string) (string, error) {
	return getGreeter().Greet(name), nil
}

type Registry struct {
	GetGreeter func() *Greeter
}

type ctxKey string

const (
	keyRegistry ctxKey = ":registry"
)

// 多分本来的には専用のoptionを見るようにすれば良い
func SetupContext(ctx context.Context) context.Context {
	registry := &Registry{
		GetGreeter: func() *Greeter { return &Greeter{} },
	}
	return context.WithValue(ctx, keyRegistry, registry)
}
func GetRegistry(ctx context.Context) *Registry {
	return ctx.Value(keyRegistry).(*Registry)
}
