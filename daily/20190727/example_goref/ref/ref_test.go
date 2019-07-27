package ref

import (
	"testing"
	"time"

	rfc3339 "github.com/podhmo/go-rfc3339"
	"github.com/podhmo/go-webtest/jsonequal"
)

type Dummy struct {
	Name string    `json:"name"`
	Now  time.Time `json:"now"`
}

func TestTransform(t *testing.T) {
	now := rfc3339.MustParse("2000-01-01T00:00:00Z")
	dummy := &Dummy{Name: "foo", Now: time.Now()}
	want := []byte(`{"name": "foo", "now": "2000-01-01T00:00:00Z"}`)

	data := jsonequal.MustNormalize(dummy)
	refs := map[string]interface{}{
		"/now": now,
	}

	if _, err := Transform(data, refs); err != nil {
		t.Error(err)
	}
	if err := jsonequal.ShouldBeSame(jsonequal.From(data), jsonequal.FromBytes(want)); err != nil {
		t.Error(err)
	}
}

func TestTransform2(t *testing.T) {
	now := rfc3339.MustParse("2000-01-01T00:00:00Z")
	data, _, _ := jsonequal.FromString(`{"a": {"b": {"c": {"now": "2011-11-11T11:11:11Z"}}}}`)()
	want := []byte(`{"a": {"b": {"c": {"now": "2000-01-01T00:00:00Z"}}}}`)

	refs := map[string]interface{}{
		"#/a/b/c/now": now,
	}

	if _, err := Transform(data, refs); err != nil {
		t.Error(err)
	}
	if err := jsonequal.ShouldBeSame(jsonequal.From(data), jsonequal.FromBytes(want)); err != nil {
		t.Error(err)
	}
}

func TestTransformBy(t *testing.T) {
	now := rfc3339.MustParse("2000-01-01T00:00:00Z")
	dummy := &Dummy{Name: "foo", Now: time.Now()}
	want := []byte(`{"name": "foo", "now": "2000-01-01T00:00:00Z"}`)

	data := jsonequal.MustNormalize(dummy)
	refs := []string{"/now"}
	pallete := jsonequal.MustNormalize(Dummy{Now: now})

	if _, err := TransformBy(data, refs, pallete); err != nil {
		t.Error(err)
	}
	if err := jsonequal.ShouldBeSame(jsonequal.From(data), jsonequal.FromBytes(want)); err != nil {
		t.Error(err)
	}
}
