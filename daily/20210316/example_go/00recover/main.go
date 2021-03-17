package main

import "errors"

func run() (err error) {
	defer func() {
		println("after")
	}()
	defer func() {
		println("recover")
		r := recover()
		if r == nil {
			println("	nil")
			return
		}
		println("	--", r)
		err = errors.New(r.(string))
	}()
	defer func() {
		println("before")
	}()
	var m map[string]int
	m["x"] = 1
	return nil
}

func main() {
	if err := run(); err != nil {
		println("!! %+v", err)
	}
}
