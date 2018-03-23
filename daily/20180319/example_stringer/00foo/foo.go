package foo

//go:generate stringer -type Foo

// Boo :
type Boo string

// Foo :
type Foo Boo

// Foo :
const (
	FooFoo = Foo("foo")
	FooBar = Foo("bar")
	FooBoo = Foo("boo")
)
