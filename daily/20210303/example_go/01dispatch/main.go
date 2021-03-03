package main

type Base struct {
}

func (b *Base) Foo() string {
	return "Base.Foo"
}
func (b *Base) Bar() string {
	return "Base.Foo"
}

type X struct {
	Base
}

func (x *X) Bar() string {
	return "X.Foo"
}

func (x *X) Foo2() string {
	return x.Foo() + " " + x.Foo()
}

func main() {
}
