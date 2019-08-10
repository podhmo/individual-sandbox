package main

import (
	"context"
	"fmt"
	"log"

	"github.com/podhmo/ctxlog"
	"github.com/podhmo/ctxlog/stdctxlog"
	"github.com/podhmo/ctxlog/zapctxlog"
	"go.uber.org/zap"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
	fmt.Println("----------------------------------------")
	if err := run2(); err != nil {
		log.Fatal(err)
	}
	fmt.Println("----------------------------------------")
	if err := run3(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	l := zapctxlog.MustNew()
	ctx, _ := ctxlog.Set(context.Background(), l).With("x-id", 10)
	return f(ctx)
}

func run2() error {
	l := zapctxlog.MustNew(
		zapctxlog.WithNewInternal2(zap.NewDevelopment),
	)
	ctx, _ := ctxlog.Set(context.Background(), l).With("x-id", 10)
	return f(ctx)
}

func run3() error {
	l := stdctxlog.New()
	ctx, _ := ctxlog.Set(context.Background(), l).With("x-id", 10)
	return f(ctx)
}

func f(ctx context.Context) error {
	ctx, log := ctxlog.Get(ctx).With("y-id", 20)
	log.Info("start f")
	defer log.Info("end f")
	return g(ctx)
}

func g(ctx context.Context) error {
	ctx, log := ctxlog.Get(ctx).With("y-id", 20)
	log.Info("start g")
	defer log.Info("end g")
	return nil
}
