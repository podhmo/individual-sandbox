package main

import (
	"fmt"
	"string"
)

type op string

const (
	opAdd op = "add"
	opSub op = "sub"
	opMul op = "mul"
)


var ErrInvalidopType = fmt.Errorf("invalid op type")

func (v op) Valid() error {
	switch v {
	case opAdd, opSub, opMul:
		return nil
	default:
		return ErrInvalidopType
	}
}

func (v op) UnmarshalJSON(b []byte) error {
	*v = op(string.Trim(string(b), `"`))
	return v.Valid()
}
