package main

import "fmt"
import "reflect"

func Lift[T any](action func()(T, error)) error {
	fmt.Println(reflect.TypeOf(action))
	result, err := action()
	if err != nil {
		return err
	}
	fmt.Println(result)
	return nil
}

func main(){
	fmt.Println(Lift(func() (int, error) { return 10, nil }))
}
