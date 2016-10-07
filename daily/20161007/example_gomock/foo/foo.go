package foo

import "fmt"

// mockgen -source ./foo.go Foo > mock_foo/mock_foo.go
type S int

const (
	// OK :
	OK = S(iota + 1)
	// NG :
	NG
)

func (s S) String() string {
	switch s {
	case OK:
		return "OK"
	case NG:
		return "NG"
	default:
		return "??"
	}
}

type Foo interface {
	Foo() S
}

type ok struct{}

func (ok *ok) Foo() S {
	return OK
}

func Use(foo Foo) string {
	return fmt.Sprintf("%s", foo.Foo())
}
