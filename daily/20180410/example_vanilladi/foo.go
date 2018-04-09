package vanilladi

// Foo :
type Foo interface {
	GetSomething() int
}

type fooConst struct{}

func (f *fooConst) GetSomething() int {
	return 42
}

// NewFooConst :
func NewFooConst() Foo {
	return &fooConst{}
}
