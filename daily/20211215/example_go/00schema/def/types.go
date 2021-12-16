package def


type Schema struct {
	Name string
}

type Name interface {
	Name() string
}

type Foo struct{}

func (f Foo) Name() string { return "foo" }

type Bar struct{}

func (f Bar) Name() string { return "bar" }

type Boo struct{}

func NewSchema(ob interface{}) *Schema {
	var name string
	if t, ok := ob.(Name); ok {
		name = t.Name()
	}
	return &Schema{Name: name}
}
