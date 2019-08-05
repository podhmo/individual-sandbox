package bar

import (
	"m/conf"
)

type Bar struct{}

func New(c *conf.Config) *Bar {
	return &Bar{}
}
