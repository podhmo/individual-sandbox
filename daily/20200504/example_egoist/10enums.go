package main

import (
	"fmt"
	"strings"
)

type op string

const (
	opAdd op = "add"
	opSub op = "sub"
	opMul op = "mul"
)


func (v op) Valid() error {
	switch v {
	case opAdd, opSub, opMul:
		return nil
	default:
		return fmt.Errorf("%q is invalid enum value of (add, sub, mul)", v)
	}
}

func (v *op) UnmarshalJSON(b []byte) error {
	*v = op(strings.Trim(string(b), `"`))
	return v.Valid()
}
