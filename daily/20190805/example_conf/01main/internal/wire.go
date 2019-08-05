//+build wireinject

package internal

import (
	"m/bar"
	"m/boo"
	"m/conf"
	"m/foo"

	"github.com/google/wire"
)

func InitializeRoot(c *conf.Config) *Root {
	wire.Build(NewRoot, foo.New, bar.New, boo.New)
	return &Root{}
}
