package main

import (
	"context"
	"fmt"

	"github.com/pkg/errors"
	"github.com/podhmo/ctxlog"
	"github.com/podhmo/ctxlog/zapctxlog"
)

func main() {
	ctx := ctxlog.Set(context.Background(), zapctxlog.MustNew())
	run(ctx)
}

func run(ctx context.Context) {
	ctx, log := ctxlog.Get(ctx).With("xxx", "yyy")
	if err := do(); err != nil {
		log.WithError(err).Warning("hmm")
		return
	}
	log.Info("hai")
	defer log.Info("h@i")
}

func do() error {
	return errors.Wrap(fmt.Errorf("!!!"), "on do")
}
