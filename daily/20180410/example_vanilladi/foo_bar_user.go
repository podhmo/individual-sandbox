package vanilladi

// FooBarUserDependency : xxx : golang's properies are not treated as the members of interface. so..
type FooBarUserDependency interface {
	DanceWithDependencies()
}

type fooBarUser struct {
	Foo Foo
	Bar Bar
}

func (x *fooBarUser) DanceWithDependencies() {
	foovalue := x.Foo.GetSomething()
	x.Bar.DoSomething(foovalue)
}

// NewFooBarUser :
func NewFooBarUser(foo Foo, bar Bar) FooBarUserDependency {
	return &fooBarUser{
		Foo: foo,
		Bar: bar,
	}
}
