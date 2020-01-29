package main

type XType string

const (
	XTypeFoo = XType("foo")
	XTypeBar = XType("bar")
	XTypeBoo = XType("boo")
)

func do(x XType, cont func(x XType) error) error {
	switch x {
	case XTypeFoo:
		return nil
	case XTypeBar:
		return nil
	case XTypeBoo:
		return cont(x)
	default:
		panic("something wrong")
	}
}

// Do ...
func Do(x XType) error {
	return do(x, func(x XType) error {
		panic("long long process, accessing external resource")
	})
}

func main() {
	Do(XTypeFoo)
	Do(XTypeBoo)
}
