package main

import (
	"context"

	"github.com/mitchellh/mapstructure"
	"github.com/podhmo/ctxlog"
	"github.com/podhmo/ctxlog/stdctxlog"
)

type Person struct {
	Name string
	Age  int
}

func main() {
	l := stdctxlog.New()
	if err := run(ctxlog.Set(context.Background(), l)); err != nil {
		l.WithError(err).Fatal("error")
	}
}

func run(ctx context.Context) error {
	data := map[string]interface{}{
		"name": "foo",
		"age":  20,
	}
	var person Person
	if err := mapstructure.Decode(data, &person); err != nil {
		return err
	}
	ctx, log := ctxlog.Get(ctx).With("pereson", person)
	log.Info("hmm")
	return nil
}
