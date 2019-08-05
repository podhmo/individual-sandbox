package boo

import (
	"m/conf"
)

type Boo struct{}

func New(c *conf.Config) *Boo {
	return &Boo{}
}
