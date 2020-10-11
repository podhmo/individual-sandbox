package main

import (
	"errors"
	"fmt"
)

type MyError struct{}

func (e MyError) Error() string {
	return "mine"
}

func run() error {
	return MyError{}
}
func main() {
	{
		var target *MyError
		err := run()
		fmt.Println(errors.As(err, &target))
	}

	// {
	// 	var target *MyError
	// 	err := run()
	// 	fmt.Println(errors.As(err, target))
	// }

	{
		err := run()
		for {
			internal := errors.Unwrap(err)
			if internal == nil {
				break
			}
			err = internal
		}

		{
			target, ok := err.(MyError)
			fmt.Println(target, ok)
		}
	}
}
