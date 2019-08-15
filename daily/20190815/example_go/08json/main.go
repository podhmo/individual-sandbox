package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"strings"
	"sync"

	"github.com/podhmo/ctxlog"
	"github.com/podhmo/ctxlog/stdctxlog"
)

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
		return err
	}

	var subs *ErrMap

	if ref.Name != nil {
		p.Name = *ref.Name
	} else { // required :
		subs = Add(subs, "name", ErrRequired)
	}
	if ref.Age != nil {
		p.Age = *ref.Age
	}
	if ref.Father != nil {
		if p.Father == nil {
			p.Father = &Person{}
		}
		if err := json.Unmarshal(*ref.Father, p.Father); err != nil {
			subs = Add(subs, "father", err)
		}
	}
	if ref.Mother != nil {
		if p.Mother == nil {
			p.Mother = &Person{}
		}
		if err := json.Unmarshal(*ref.Mother, p.Mother); err != nil {
			subs = Add(subs, "mother", err)
		}
	}
	return subs.OrNil()
}

// TransparentErr :
type TransparentErr struct {
	Summary  string
	Messages []string
}

// Error :
func (e *TransparentErr) Error() string {
	return e.Summary
}

// MarshalJSON :
func (e *TransparentErr) MarshalJSON() ([]byte, error) {
	// TODO: verbose
	return []byte(fmt.Sprintf(`%q`, e.Summary)), nil
}

var (
	// ErrRequired :
	ErrRequired = &TransparentErr{Summary: "required"}
)

// ErrMap :
type ErrMap struct {
	mu  sync.Mutex
	Map map[string]error

	Fields []string
}

// OrNil :
func (e *ErrMap) OrNil() error {
	if e == nil {
		return nil
	}
	return e
}

// Add :
func (e *ErrMap) Add(name string, err error) *ErrMap {
	defer e.mu.Unlock()
	e.mu.Lock()
	e.Fields = append(e.Fields, name)
	e.Map[name] = err
	return e
}

// Error :
func (e *ErrMap) Error() string {
	var b strings.Builder
	encoder := json.NewEncoder(&b)
	if err := encoder.Encode(e); err != nil {
		for _, name := range e.Fields {
			fmt.Fprintln(&b, name, e.Map[name].Error())
		}
	}
	return b.String()
}

// MarshalJSON :
func (e *ErrMap) MarshalJSON() ([]byte, error) {
	var b bytes.Buffer
	encoder := json.NewEncoder(&b)
	b.WriteString("{")
	for i, name := range e.Fields {
		b.WriteString(`"` + name + `": `)
		if err := encoder.Encode(e.Map[name]); err != nil {
			return nil, err // xxx
		}
		if i < len(e.Fields)-1 {
			b.WriteString(",")
		}
	}
	b.WriteString("}")
	return b.Bytes(), nil
}

// New :
func New() *ErrMap {
	return &ErrMap{
		Map: map[string]error{},
	}
}

// Add :
func Add(root *ErrMap, name string, err error) *ErrMap {
	if root == nil {
		root = New()
	}
	return root.Add(name, err)
}

func main() {
	// log := zapctxlog.MustNew(zapctxlog.WithNewInternal2(zap.NewDevelopment))
	log := stdctxlog.New()
	ctx := context.Background()
	if err := run(ctxlog.Set(ctx, log)); err != nil {
		log.WithError(err).Info("x")
	}
}

func run(ctx context.Context) error {
	body := `{"name": "foo", "age": 20, "father": {"age": 40}, "mother": {"age": 40}}`
	var ob Person
	if err := json.Unmarshal([]byte(body), &ob); err != nil {
		return err
	}
	ctxlog.Get(ctx).Info("hmm", "ob", ob)
	return nil
}
