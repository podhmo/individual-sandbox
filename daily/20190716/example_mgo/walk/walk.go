package walk

import (
	"fmt"
	"reflect"
	"strings"
	"time"

	"github.com/pkg/errors"
	"gopkg.in/mgo.v2/bson"
)

var (
	ErrInvalidKey = fmt.Errorf("invalid key")
)

// Walk :
func Walk(m bson.M) error {
	if len(m) == 0 {
		return nil
	}
	if err := walkInternal(m); err != nil {
		return errors.WithMessage(err, "invalied")
	}
	return nil
}

func isInvalid(key string) bool {
	if !strings.Contains(key, "ID") {
		return false
	}
	if !strings.Contains(key, ".") {
		return strings.HasSuffix(key, "ID")
	}
	for _, x := range strings.Split(key, ".") {
		if strings.HasSuffix(x, "ID") {
			return true
		}
	}
	return false
}

func walkInternal(m interface{}) error {
	switch m := m.(type) {
	case bson.M:
		return walkInternal((map[string]interface{})(m))
	case map[string]interface{}:
		for k, v := range m {
			if isInvalid(k) {
				return errors.Wrapf(ErrInvalidKey, "%q", k)
			}
			if err := walkInternal(v); err != nil {
				return errors.WithMessagef(err, "%q", k)
			}
		}
	case uint, uint8, uint16, uint32, uint64, int8, int16, int, int32, int64, float32, float64, time.Time:
		return nil
	case reflect.Value:
		switch m.Kind() {
		case reflect.Array:
			if m.Len() == 0 {
				return nil
			}
			for i := 0; i < m.Len(); i++ {
				if err := walkInternal(m.Index(i)); err != nil {
					return errors.WithMessagef(err, "%d", i)
				}
			}
			return nil
		case reflect.Slice:
			if m.IsNil() {
				return nil
			}
			if m.CanAddr() {
				return walkInternal(m.Elem())
			}
			if m.Len() == 0 {
				return nil
			}
			for i := 0; i < m.Len(); i++ {
				if err := walkInternal(m.Index(i)); err != nil {
					return errors.WithMessagef(err, "%d", i)
				}
			}
			return nil
		case reflect.Map:
			if m.IsNil() {
				return nil
			}
			if m.CanAddr() {
				return walkInternal(m.Elem())
			}
			if m.Len() == 0 {
				return nil
			}
			for _, k := range m.MapKeys() {
				if err := walkInternal(k); err != nil {
					return errors.WithMessagef(err, "%v", k)
				}
				v := m.MapIndex(k)
				if !v.IsValid() {
					return nil
				}
				if err := walkInternal(v); err != nil {
					return errors.WithMessagef(err, "%v", k) // xxx
				}
			}
		case reflect.Ptr:
			if m.IsNil() {
				return nil
			}
			return walkInternal(m.Elem())
		case reflect.Interface:
			return nil
		// case reflect.Struct:
		default:
			return nil
		}
	default:
		return walkInternal(reflect.ValueOf(m))
	}
	return nil
}
