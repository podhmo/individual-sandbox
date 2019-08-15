package main

import (
	"github.com/podhmo/ctxlog/zapctxlog"
	"go.uber.org/zap"
)

func main() {
	l, err := zapctxlog.New(zapctxlog.WithNewInternal2(zap.NewDevelopment))
	if err != nil {
		panic(err)
	}
	l = l.With("x", "before", "y", "before")
	l.Info("heh", "x", "after", "z", "after")
}
