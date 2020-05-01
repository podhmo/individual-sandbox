package main

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

func main() {
	{
		v := Op("Add")
		fmt.Printf("%s, %#+v\n", v, v)
	}
	{
		v := Op("Sub")
		fmt.Printf("%s, %#+v\n", v, v)
	}
	{
		v := Op("Mul")
		fmt.Printf("%s, %#+v\n", v, v)
	}
	{
		var v Op
		if err := json.Unmarshal([]byte(`"Mul"`), &v); err != nil {
			fmt.Println("!", err)
		}
		fmt.Printf("%s, %#+v\n", v, v)
	}
	{
		b, err := json.Marshal(map[string]interface{}{"Mul": OpMul, "s": "Mul"})
		if err != nil {
			fmt.Println("!", err)
		}
		fmt.Printf("%s\n", string(b))
	}
}
