package internal

import (
	"m/bar"
	"m/boo"
	"m/foo"
)

type Root struct {
	Foo *foo.Foo
	Bar *bar.Bar
	Boo *boo.Boo
}

func NewRoot(foo *foo.Foo, bar *bar.Bar, boo *boo.Boo) *Root {
	return &Root{Foo: foo, Bar: bar, Boo: boo}
}
