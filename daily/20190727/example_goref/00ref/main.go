package main

import (
	"log"
	"time"

	"github.com/k0kubun/pp"
	rfc3339 "github.com/podhmo/go-rfc3339"
	"github.com/podhmo/go-webtest/jsonequal"
)

// Result :
type Result struct {
	Data interface{}
	Refs map[string]interface{}
}

func run() error {
	type Dummy struct {
		Name string    `json:"name"`
		Now  time.Time `json:"now"`
	}

	now := rfc3339.MustParse("2000-01-01T00:00:00Z")
	dummy := &Dummy{Name: "foo", Now: time.Now()}

	result := Result{
		Data: jsonequal.MustNormalize(dummy),
		Refs: map[string]interface{}{
			"now": now,
		},
	}
	pp.Println(result)

	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}
