package nullable

import (
	"bytes"
	"database/sql/driver"
	"encoding/json"
	"fmt"
	"reflect"
	"time"
	_ "unsafe"
)

//go:linkname database_sql__convertAssign database/sql.convertAssign
func database_sql__convertAssign(dest, src any) error

type constraint interface {
	~string | ~[]byte |
		~int64 | ~int32 | ~int | ~int16 | ~int8 |
		~uint64 | ~uint32 | ~uint | ~uint16 | ~uint8 | ~float64 |
		~bool | time.Time

	comparable
}

type Type[T constraint] struct {
	value T
	Valid bool
}

func (t *Type[T]) UnmarshalJSON(b []byte) error {
	if b == nil || bytes.Equal(nullValue, b) {
		return nil
	}
	t.Valid = true
	return json.Unmarshal(b, &t.value) // TODO:performance improvement
}

func (t Type[T]) MarshalJSON() ([]byte, error) {
	if !t.Valid {
		return nullValue, nil
	}
	return json.Marshal(t.value) // TODO: performance improvement
}

func (t *Type[T]) Scan(value any) error {
	if value == nil {
		var z T
		t.value = z
		t.Valid = false
		return nil
	}
	t.Valid = true
	return database_sql__convertAssign(&t.value, value)
}

func (t Type[T]) Value() (driver.Value, error) {
	if !t.Valid {
		return nil, nil
	}

	// ONLY: int64 | float64 | bool | []byte | string | time.Time
	rv := reflect.ValueOf(t.value)
	switch rv.Kind() {
	case reflect.Bool:
		return rv.Bool(), nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return int64(rv.Int()), nil
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32:
		return int64(rv.Uint()), nil
	case reflect.Uint64:
		return int64(rv.Uint()), nil // XXX: overflow
	case reflect.Float32, reflect.Float64:
		return float64(rv.Float()), nil
	case reflect.Slice:
		if rv.Type().Elem() == rBytesType {
			return rv.Bytes(), nil
		}
		return nil, fmt.Errorf("unexpected type: %v", rv.Type())
	case reflect.String:
		return rv.String(), nil
	// case reflect.Complex64:
	// case reflect.Complex128:
	// case reflect.Array:
	// case reflect.Chan:
	// case reflect.Func:
	// case reflect.Interface:
	// case reflect.Map:
	// case reflect.Pointer:
	// case reflect.Struct:
	// case reflect.UnsafePointer:
	default:
		if rv.Type() == rTimeType {
			return rv.Interface().(time.Time), nil
		}
		return nil, fmt.Errorf("unexpected type: %v", rv.Type())
	}
}

func New[T constraint](value T) Type[T] {
	return Type[T]{
		value: value,
		Valid: true,
	}
}

var nullValue = []byte(`null`)
var rBytesType = reflect.TypeOf(func([]byte) {}).In(0)
var rTimeType = reflect.TypeOf(func(time.Time) {}).In(0)
