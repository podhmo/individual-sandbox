package main

import (
	"fmt"
	"strings"
)

type V uint8

// https://github.com/uber-go/guide/blob/master/style.md#start-enums-at-one
const (
	VX = iota + 1
	VY
	VZ
)

func (v V) String() string {
	switch v {
	case VX:
		return "X"
	case VY:
		return "Y"
	case VZ:
		return "Z"
	default:
		return "?"
	}
}

func ParseV(s string) (V, error) {
	switch strings.ToUpper(s) {
	case "X":
		return VX, nil
	case "Y":
		return VY, nil
	case "Z":
		return VZ, nil
	default:
		return 0, fmt.Errorf("unexpected V %q", s)
	}
}

func MustV(s string) V {
	v, err := ParseV(s)
	if err != nil {
		panic(err)
	}
	return v
}

func main() {
	fmt.Printf("%s, %#+v\n", MustV("x"), MustV("x"))
	fmt.Printf("%s, %#+v\n", MustV("y"), MustV("y"))
	fmt.Printf("%s, %#+v\n", MustV("z"), MustV("z"))
}
