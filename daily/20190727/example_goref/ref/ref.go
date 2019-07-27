package ref

import (
	"strings"

	"github.com/pkg/errors"
	"github.com/xeipuuv/gojsonpointer"
)

// Transform : (side-effect !!)
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
