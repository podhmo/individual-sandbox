package enums

import (
    "encoding/json"
    "fmt"
    "strings"

    "github.com/pkg/errors"
)

type Op string

const (
    OpAdd Op = "Add"
    OpSub    = "Sub"
    OpMul    = "Mul"
)

var ErrInvalidOpType = fmt.Errorf("invalid op type")

func (v Op) String() string {
    return string(v)
}

func (v Op) Valid() error {
    switch v {
    case OpAdd, OpSub, OpMul:
        return nil
    default:
        return errors.Wrapf(ErrInvalidOpType, "get %s", v)
    }
}

func (v *Op) UnmarshalJSON(b []byte) error {
    *v = Op(strings.Trim(string(b), `"`))
    return v.Valid()
}

func ParseOp(s string) (v Op, err error) {
    v = Op(s)
    err = v.Valid()
    return
}
