package main

import "fmt"

type M map[string]interface{}

func main() {
	{
		var iface interface{}
		iface = M(map[string]interface{}{"foo": 1})
		run(iface)
	}
	{
		var iface interface{}
		iface = map[string]interface{}{"foo": 1}
		run(iface)
	}
}

func run(iface interface{}) {
	switch iface.(type) {
	case M:
		fmt.Println("this type")
	case map[string]interface{}:
		fmt.Println("underlying type")
	}

	switch iface.(type) {
	case M, map[string]interface{}:
		fmt.Println("this type2")
	default:
		fmt.Println("else")
	}
}
