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

type N string

const (
	NI = N("i")
	NJ = N("j")
	NK = N("k")
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

func ParseN(s string, n interface{}) error {
	if IsN(s) {
		*((n).(*N)) = N(s)
		return nil
	}
	return fmt.Errorf("unexpected nalue %q", s)
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
	{
		var n N
		Must(ParseN, "i", &n)
		fmt.Printf("%s, %#+v\n", n, n)
	}
	{
		var n N
		Must(ParseN, "j", &n)
		fmt.Printf("%s, %#+v\n", n, n)
	}
	{
		var n N
		Must(ParseN, "k", &n)
		fmt.Printf("%s, %#+v\n", n, n)
	}
}
