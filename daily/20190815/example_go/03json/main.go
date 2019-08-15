package main

import (
	"context"
	"encoding/json"

	"github.com/podhmo/ctxlog"
	"github.com/podhmo/ctxlog/zapctxlog"
	"go.uber.org/zap"
)

func main() {
	log := zapctxlog.MustNew(zapctxlog.WithNewInternal2(zap.NewDevelopment))
	ctx := context.Background()
	if err := run(ctxlog.Set(ctx, log)); err != nil {
		log.WithError(err).Fatal("x")
	}
}

// Person :
type Person struct {
	Name   string // required
	Age    int
	Father *Person
	Mother *Person
}

func run(ctx context.Context) error {
	body := `{"name": "foo", "age": 20, "father": {"age": 40}}`
	var ob Person
	json.Unmarshal([]byte(body), &ob)
	ctxlog.Get(ctx).Info("hmm", "ob", ob)
	return nil
}
