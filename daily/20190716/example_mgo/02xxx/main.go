package main

import (
	"errors"
	"fmt"
	"reflect"
)

func main() {
	xs := []int{1, 2, 3}
	fmt.Println(run(xs))
}

func run(v interface{}) error {
	switch v := v.(type) {
	case []interface{}:
		fmt.Println("ints", v)
		return nil
	default:
		rv := reflect.ValueOf(v)
		switch rv.Kind() {
		case reflect.Slice:
			for i := 0; i < rv.Len(); i++ {
				if err := run(rv.Index(i)); err != nil {
					return err
				}
			}
			return nil
		default:
			return errors.New("hmm")
		}
	}
}
