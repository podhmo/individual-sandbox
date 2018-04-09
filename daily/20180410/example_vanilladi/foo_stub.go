package vanilladi

type fooStub struct {
	nextValue int
}

// GetSomething :
func (f *fooStub) GetSomething() int {
	return f.nextValue
}

// NewFooStub :
func NewFooStub(value int) Foo {
	return &fooStub{nextValue: value}
}
