package main

import (
	"fmt"
	"github.com/pkg/errors"
	"os"
)

func f0() error {
	err := f1()
	if err != nil {
		return errors.Wrapf(err, "f0")
	}
	return err
}
func f1() error {
	err := f2()
	if err != nil {
		return errors.Wrapf(err, "f1")
	}
	return err
}
func f2() error {
	err := f3()
	if err != nil {
		return errors.Wrapf(err, "f2")
	}
	return err
}
func f3() error {
	return fmt.Errorf("*error on a external package*")
}

func main() {
	type causer interface {
		Cause() error
	}
	type stackTracer interface {
		StackTrace() errors.StackTrace
	}

	err := f0()
	if err != nil {
        errs := []stackTracer{}
		for err != nil {
			if err, ok := err.(stackTracer); ok {
				errs = append(errs, err)
			}

			cause, ok := err.(causer)
			if !ok {
				break
			}
			err = cause.Cause()
		}
        fmt.Println("stack trace")
		for _, frame := range errs[len(errs)-1].StackTrace() {
			fmt.Printf("\t %+v\n", frame)
		}
		os.Exit(1)
	}
}
