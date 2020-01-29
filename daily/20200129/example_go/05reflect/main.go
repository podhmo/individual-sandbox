package main

import (
	"fmt"
	"reflect"
	"strings"
	"unsafe"
)

type S struct {
	cont func(indent int) error
}

func (s *S) Do() error {
	fmt.Println("Run")
	return s.cont(1)
}

func New() *S {
	return &S{
		cont: func(n int) error {
			fmt.Println(strings.Repeat("\t", n), "end")
			return nil
		},
	}
}

func main() {
	{
		s := New()
		fmt.Println(s.Do())
	}
	fmt.Println("----------------------------------------")

	{
		s := New()

		rv := reflect.ValueOf(s).Elem()
		rt := rv.Type()

		var rf reflect.Value
		for i := 0; i < rt.NumField(); i++ {
			f := rt.Field(i)
			if f.Name == "cont" {
				rf = rv.FieldByIndex([]int{i})
				break
			}
		}

		ptr := unsafe.Pointer(rf.UnsafeAddr())
		realPtr := (*func(int) error)(ptr)
		cont := func(n int) error {
			fmt.Println(strings.Repeat("@", n*4), "<black magic>")
			return fmt.Errorf("hmm")
		}
		*realPtr = cont

		fmt.Println(s.Do())
	}

}
