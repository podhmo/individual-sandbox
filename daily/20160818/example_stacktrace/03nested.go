package main

import (
	"github.com/pkg/errors"
	"fmt"
	"os"
)

func f0() error{
    err := f1()
    if err != nil {
        return errors.Wrapf(err, "f0")
    }
    return err
}
func f1() error{
    err := f2()
    if err != nil {
        return errors.Wrapf(err, "f1")
    }
    return err
}
func f2() error{
    err := f3()
    if err != nil {
        return errors.Wrapf(err, "f2")
    }
    return err
}
func f3() error{
    return fmt.Errorf("*error on a external package*")
}

func main() {
    err := f0()
    if err != nil {
        fmt.Printf("err %+v\n", err)
        os.Exit(1)
    }
}
