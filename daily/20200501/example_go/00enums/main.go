package main

import "fmt"

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
		return ":X"
	case VY:
		return ":Y"
	case VZ:
		return ":Z"
	default:
		return ":?"
	}
}

func IsV(n uint8) bool {
	switch V(n) {
	case VX:
		return true
	case VY:
		return true
	case VZ:
		return true
	default:
		return false
	}
}

func ParseV(n uint8) (V, error) {
	if IsV(n) {
		return V(n), nil
	}
	return 0, fmt.Errorf("unexpected value %v", n)
}

func MustV(n uint8) V {
	v, err := ParseV(n)
	if err != nil {
		panic(err)
	}
	return v
}

func main() {
	fmt.Printf("%s, %#+v\n", MustV(1), MustV(1))
	fmt.Printf("%s, %#+v\n", MustV(2), MustV(2))
	fmt.Printf("%s, %#+v\n", MustV(3), MustV(3))
}
