package main

import (
	"fmt"
	"reflect"
)

func main() {
	{
		type S struct {
			Name string
		}
		x := S{}
		y := S{}
		fmt.Println(x, y, x == y, reflect.DeepEqual(x, y))
	}
	{
		type S struct {
			Name *string
		}
		x := S{}
		y := S{}
		fmt.Println(x, y, x == y, reflect.DeepEqual(x, y))
	}
	{
		type S struct {
			Sub struct {
				Name string
			}
		}
		x := S{}
		y := S{}
		fmt.Println(x, y, x == y, reflect.DeepEqual(x, y))
	}
	{
		type S struct {
			Sub struct {
				Name *string
			}
		}
		x := S{}
		y := S{}
		fmt.Println(x, y, x == y, reflect.DeepEqual(x, y))
	}
}
