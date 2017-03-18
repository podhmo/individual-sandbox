package main

import (
	"errors"
	"fmt"
)

func f() (int, error) {
	return 1, errors.New("f")
}
func g(err error) (error, error) {
	return err, errors.New("g")
}

func main() {
	{
		x, err := f()
		fmt.Println(x, err)
		err, err2 := g(err)
		fmt.Println(err, err2)
		err, err3 := g(err2)
		fmt.Println(err, err3)
		fmt.Println(x, err)

	}
	fmt.Println("----------------------------------------")
	{
		x, err := f()
		fmt.Println(x, err)
		{
			err, err2 := g(err)
			fmt.Println(err, err2)
			err, err3 := g(err2)
			fmt.Println(err, err3)
		}
		fmt.Println(x, err)

	}
}
