package main

import (
	"fmt"
	"log"
	"strings"
	"time"

	"github.com/k0kubun/pp"
	"github.com/pkg/errors"
	rfc3339 "github.com/podhmo/go-rfc3339"
	"github.com/podhmo/go-webtest/jsonequal"
	"github.com/xeipuuv/gojsonpointer"
)

// Transform :
func Transform(data interface{}, refs map[string]interface{}) (interface{}, error) {
	for k, v := range refs {
		jptr, err := gojsonpointer.NewJsonPointer(strings.TrimPrefix(k, "#"))
		if err != nil {
			return nil, errors.WithMessagef(err, "parse %q as jsonpointer", k)
		}
		if _, err := jptr.Set(data, v); err != nil {
			return nil, errors.WithMessagef(err, "access %q on data (set)", k)
		}
	}
	return data, nil
}

// TransformBy : (side-effect !!)
func TransformBy(data interface{}, refs []string, palette interface{}) (interface{}, error) {
	for _, k := range refs {
		jptr, err := gojsonpointer.NewJsonPointer(strings.TrimPrefix(k, "#"))
		if err != nil {
			return nil, errors.WithMessagef(err, "parse %q as jsonpointer", k)
		}
		v, _, err := jptr.Get(palette)
		if err != nil {
			return nil, errors.WithMessagef(err, "access %q on pallete (get)", k)
		}
		if _, err := jptr.Set(data, v); err != nil {
			return nil, errors.WithMessagef(err, "access %q on data (set)", k)
		}
	}
	return data, nil
}

func run() error {
	type Dummy struct {
		Name string    `json:"name"`
		Now  time.Time `json:"now"`
	}

	now := rfc3339.MustParse("2000-01-01T00:00:00Z")
	dummy := &Dummy{Name: "foo", Now: time.Now()}
	want := []byte(`{"name": "foo", "now": "2000-01-01T00:00:00Z"}`)

	{
		data := jsonequal.MustNormalize(dummy)
		refs := map[string]interface{}{
			"/now": now,
		}

		pp.Println(data, refs)
		fmt.Println("----------------------------------------")
		if _, err := Transform(data, refs); err != nil {
			return err
		}
		pp.Println(data, refs)

		// check
		if err := jsonequal.ShouldBeSame(jsonequal.From(data), jsonequal.FromBytes(want)); err != nil {
			return err
		}
	}

	fmt.Println("========================================")
	{
		data := jsonequal.MustNormalize(dummy)
		refs := []string{"/now"}
		pallete := jsonequal.MustNormalize(Dummy{Now: now})
		pp.Println(data, refs)
		fmt.Println("----------------------------------------")
		if _, err := TransformBy(data, refs, pallete); err != nil {
			return err
		}
		pp.Println(data, refs)

		// check
		if err := jsonequal.ShouldBeSame(jsonequal.From(data), jsonequal.FromBytes(want)); err != nil {
			return err
		}
	}
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}
