package main

import (
	"fmt"
)

func f()(s string, err error) {
    s, err = g()
    defer func(){
        if cerr := cleanup(s); err == nil && cerr != nil {
            err = cerr
        }
    }()
    return
}

func g()(string, error) {
    return "ok", nil
}

func cleanup(s string) error {
    return fmt.Errorf("hmm")
}

func main(){
    fmt.Println(f())
}
