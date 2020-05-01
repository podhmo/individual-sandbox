package main

import "fmt"

type V string

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

func main() {
	fmt.Printf("%s, %#+v\n", MustV("x"), MustV("x"))
	fmt.Printf("%s, %#+v\n", MustV("y"), MustV("y"))
	fmt.Printf("%s, %#+v\n", MustV("z"), MustV("z"))
}
