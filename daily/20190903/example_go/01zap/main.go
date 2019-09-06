package main

import (
	"context"

	"github.com/pkg/errors"
	"github.com/podhmo/ctxlog"
	"github.com/podhmo/ctxlog/zapctxlog"
)

func main() {
	l := zapctxlog.MustNew()
	f(ctxlog.Set(context.Background(), l))
}

func f(ctx context.Context) {
	if err := g(ctx); err != nil {
		ctxlog.Get(ctx).Info("hmm", "error", err)
	}
}

func g(ctx context.Context) error {
	return errors.Errorf("ng")
}
