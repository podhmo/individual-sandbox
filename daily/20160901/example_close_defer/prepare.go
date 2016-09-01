package main

import (
	"fmt"
)

func run() error {
	var err error
	defer func() {
		err = fmt.Errorf("oops")
	}()
	return err
}

func run2() (err error) {
	defer func() {
		err = fmt.Errorf("oops")
	}()
	return err
}

func main() {
	fmt.Printf("%[1]T: %[1]v\n", run())  // <nil>: <nil>
	fmt.Printf("%[1]T: %[1]v\n", run2()) // *errors.errorString: oops
}
