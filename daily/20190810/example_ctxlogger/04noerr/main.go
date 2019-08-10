package main

import (
	"context"

	"github.com/podhmo/ctxlog"
)

func main() {
	run(context.Background())
}

func run(ctx context.Context) {
	ctxlog.Get(ctx).Info("hmm")
}
