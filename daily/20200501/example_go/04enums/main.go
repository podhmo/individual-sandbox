package main

import "fmt"

type V string

func (v V) String() string {
	return string(v)
}

const (
	VX = V("x")
	VY = V("y")
	VZ = V("z")
)

func IsV(s string) bool {
	switch s {
	case "x":
		return true
	case "y":
		return true
	case "z":
		return true
	default:
		return false
	}

}

func ParseV(s string) (V, error) {
	if IsV(s) {
		return V(s), nil
	}
	return "", fmt.Errorf("unexpected value %v", s)
}

func MustV(s string) V {
	v, err := ParseV(s)
	if err != nil {
		panic(err)
	}
	return v
}

type N string

func (n N) String() string {
	return string(n)
}

const (
	NX = N("i")
	NY = N("j")
	NZ = N("k")
)

func IsN(s string) bool {
	switch s {
	case "i":
		return true
	case "j":
		return true
	case "k":
		return true
	default:
		return false
	}

}

func ParseN(s string) (N, error) {
	if IsN(s) {
		return N(s), nil
	}
	return "", fmt.Errorf("unexpected nalue %v", s)
}

func MustN(s string) N {
	n, err := ParseN(s)
	if err != nil {
		panic(err)
	}
	return n
}

func main() {
	fmt.Printf("%s, %#+v\n", MustV("x"), MustV("x"))
	fmt.Printf("%s, %#+v\n", MustV("y"), MustV("y"))
	fmt.Printf("%s, %#+v\n", MustV("z"), MustV("z"))

	fmt.Printf("%s, %#+v\n", MustN("i"), MustN("i"))
	fmt.Printf("%s, %#+v\n", MustN("j"), MustN("j"))
	fmt.Printf("%s, %#+v\n", MustN("k"), MustN("k"))
}
