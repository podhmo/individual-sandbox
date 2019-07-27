package main

import (
	"fmt"
	"log"
	"m/ref"
	"time"

	"github.com/k0kubun/pp"
	rfc3339 "github.com/podhmo/go-rfc3339"
	"github.com/podhmo/go-webtest/jsonequal"
)

func run() error {
	type Dummy struct {
		Name string    `json:"name"`
		Now  time.Time `json:"now"`
	}

	now := rfc3339.MustParse("2000-01-01T00:00:00Z")
	dummy := &Dummy{Name: "foo", Now: time.Now()}
	want := []byte(`{"name": "foo", "now": "2000-01-01T00:00:00Z"}`)

	data := jsonequal.MustNormalize(dummy)
	refs := map[string]interface{}{
		"/now": now,
	}

	pp.Println(data, refs)
	fmt.Println("----------------------------------------")
	if _, err := ref.Transform(data, refs); err != nil {
		return err
	}
	pp.Println(data, refs)

	// check
	if err := jsonequal.ShouldBeSame(jsonequal.From(data), jsonequal.FromBytes(want)); err != nil {
		return err
	}

	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}
