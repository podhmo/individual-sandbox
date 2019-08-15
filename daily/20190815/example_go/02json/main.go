package main

import (
	"context"
	"encoding/json"

	"github.com/podhmo/ctxlog"
	"github.com/podhmo/ctxlog/stdctxlog"
)

func main() {
	log := stdctxlog.New()
	ctx := context.Background()
	if err := run(ctxlog.Set(ctx, log)); err != nil {
		log.WithError(err).Fatal("x")
	}
}

// Person :
type Person struct {
	Name string
	Age  int
}

func run(ctx context.Context) error {
	body := `{"name": "foo", "age": 20}`
	var ob Person
	json.Unmarshal([]byte(body), &ob)
	ctxlog.Get(ctx).Info("hmm", "ob", ob)
	return nil
}
