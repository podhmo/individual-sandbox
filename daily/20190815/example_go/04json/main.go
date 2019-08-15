package main

import (
	"context"
	"encoding/json"

	"github.com/pkg/errors"
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

// UnmarshalJSON :
func (p *Person) UnmarshalJSON(b []byte) error {
	var ref struct {
		Name   *string
		Age    *int
		Father *json.RawMessage
		Mother *json.RawMessage
	}
	if err := json.Unmarshal(b, &ref); err != nil {
		return errors.WithMessage(err, "person")
	}
	if ref.Name != nil {
		p.Name = *ref.Name
	}
	if ref.Age != nil {
		p.Age = *ref.Age
	}
	if ref.Father != nil {
		if p.Father == nil {
			p.Father = &Person{}
		}
		if err := json.Unmarshal(*ref.Father, p.Father); err != nil {
			return errors.WithMessage(err, "father")
		}
	}
	if ref.Mother != nil {
		if p.Mother == nil {
			p.Mother = &Person{}
		}
		if err := json.Unmarshal(*ref.Mother, p.Mother); err != nil {
			return errors.WithMessage(err, "mother")
		}
	}
	return nil
}

func run(ctx context.Context) error {
	body := `{"name": "foo", "age": 20, "father": {"age": 40}}`
	var ob Person
	if err := json.Unmarshal([]byte(body), &ob); err != nil {
		return err
	}
	ctxlog.Get(ctx).Info("hmm", "ob", ob)
	return nil
}
