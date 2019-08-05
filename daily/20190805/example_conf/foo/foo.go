package foo

import (
	"m/conf"
)

type Foo struct{}

func New(c *conf.Config) *Foo {
	return &Foo{}
}
