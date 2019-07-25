package jsonequal

import (
	"encoding/json"
	"fmt"
	"io"
	"reflect"

	"github.com/pkg/errors"
)

// Caller :
type Caller struct {
	EqualFunc func(left interface{}, right interface{}) bool
	WrapfFunc func(error, string, ...interface{}) error
	FailFunc  func(left interface{}, right interface{}, lb []byte, rb []byte) error
}

// From :
func From(iface interface{}) func() (interface{}, []byte, error) {
	return func() (interface{}, []byte, error) {
		b, err := json.Marshal(iface)
		if err != nil {
			return nil, nil, err
		}
		var v interface{}
		if err := json.Unmarshal(b, &v); err != nil {
			return nil, nil, err
		}
		return v, b, nil
	}
}

// FromReader :
func FromReader(reader io.Reader) func() (interface{}, []byte, error) {
	return func() (interface{}, []byte, error) {
		decoder := json.NewDecoder(reader)
		var v interface{}
		if err := decoder.Decode(&v); err != nil {
			return nil, nil, err
		}
		b, err := json.Marshal(&v)
		if err != nil {
			return nil, nil, err
		}
		return v, b, nil
	}
}

// FromBytes :
func FromBytes(b []byte) func() (interface{}, []byte, error) {
	return func() (interface{}, []byte, error) {
		var v interface{}
		if err := json.Unmarshal(b, &v); err != nil {
			return nil, nil, err
		}
		return v, b, nil
	}
}

// Equal :
func Equal(
	lsrc func() (interface{}, []byte, error),
	rsrc func() (interface{}, []byte, error),
	options ...func(*Caller),
) error {
	caller := Caller{}
	for _, opt := range options {
		opt(&caller)
	}
	if caller.EqualFunc == nil {
		caller.EqualFunc = reflect.DeepEqual
	}
	if caller.WrapfFunc == nil {
		caller.WrapfFunc = errors.WithMessagef
	}
	if caller.FailFunc == nil {
		caller.FailFunc = defaultFail
	}

	lv, lb, err := lsrc()
	if err != nil {
		return caller.WrapfFunc(err, "left")
	}
	rv, rb, err := rsrc()
	if err != nil {
		return caller.WrapfFunc(err, "right")
	}

	if !caller.EqualFunc(lv, rv) {
		return caller.WrapfFunc(caller.FailFunc(lv, rv, lb, rb), "equal")
	}
	return nil
}

func defaultFail(
	left interface{},
	right interface{},
	lb []byte,
	rb []byte,
) error {
	// todo : more redable expression
	msg := "not equal json\nleft:\n	%s\nright:\n	%s\n"
	return fmt.Errorf(msg, string(lb), string(rb))
}
