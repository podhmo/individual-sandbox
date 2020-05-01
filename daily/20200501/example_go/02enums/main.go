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

func ParseV(s string, v interface{}) error {
	if IsV(s) {
		*((v).(*V)) = V(s)
		return nil
	}
	return fmt.Errorf("unexpected value %q", s)
}

func Must(parse func(s string, v interface{}) error, s string, v interface{}) {
	if err := parse(s, v); err != nil {
		panic(err)
	}
}

func main() {
	{
		var v V
		Must(ParseV, "x", &v)
		fmt.Printf("%s, %#+v\n", v, v)
	}
	{
		var v V
		Must(ParseV, "y", &v)
		fmt.Printf("%s, %#+v\n", v, v)
	}
	{
		var v V
		Must(ParseV, "z", &v)
		fmt.Printf("%s, %#+v\n", v, v)
	}
}
