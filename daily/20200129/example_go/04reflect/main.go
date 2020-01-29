package main

import (
	"fmt"
	"reflect"
	"unsafe"

	"github.com/k0kubun/pp"
)

// S ...
type S struct {
	Exported   string
	unexported string
}

func main() {
	s := &S{}

	rv := reflect.ValueOf(s).Elem()

	fmt.Println("by name")
	{
		{
			rf := rv.FieldByName("Exported")
			fmt.Printf("	Exported: %[1]T%+[1]v\n", rf)
			rf.SetString("change by name")
		}
		{
			rf := rv.FieldByName("uexported")
			fmt.Printf("	unexported: %[1]T%+[1]v\n", rf)
			// rf.SetString("change by name") // error
		}
		pp.Println(s)
	}

	fmt.Println("by index")
	{
		rt := rv.Type()
		{
			for i := 0; i < rt.NumField(); i++ {
				f := rt.Field(i)
				if f.Name == "Exported" {
					rf := rv.FieldByIndex([]int{i})
					fmt.Printf("	Exported: %[1]T%+[1]v\n", rf)
					rf.SetString("change by index")
					break
				}
			}
		}
		{
			for i := 0; i < rt.NumField(); i++ {
				f := rt.Field(i)
				if f.Name == "unexported" {
					rf := rv.FieldByIndex([]int{i})
					fmt.Printf("	unexported: %[1]T%+[1]v\n", rf)

					// black magic
					ptr := unsafe.Pointer(rf.UnsafeAddr())
					realPtr := (*string)(ptr)
					val := "<black magic>"
					*realPtr = val
					break
				}
			}
		}
		pp.Println(s)
	}
}
