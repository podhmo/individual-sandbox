package main

import (
	"m/FFF"
	"m/SSS"
	"reflect"

	"github.com/k0kubun/pp"
)

func main() {
	{
		rv := reflect.ValueOf(FFF.F)
		rt := rv.Type()

		pp.Println(rv)
		pp.Println(rt)
	}

	{
		rv := reflect.ValueOf(SSS.S{})
		rt := rv.Type()

		pp.Println(rv)
		pp.Println(rt)
	}
}
