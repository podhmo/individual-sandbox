package foo

type Namer interface {
	Name() string
}

type foo struct {}
func (f *foo) Name() string { return "foo" }
func Foo() *foo { return &foo{} }

type bar struct {}
func (b *bar) Name() string { return "bar" }
func Bar() *bar { return &bar{} }


type boo struct { }
