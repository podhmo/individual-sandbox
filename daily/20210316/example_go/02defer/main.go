package main

import "fmt"

func run() (err error) {
	useX := false
	defer func() {
		fmt.Println("okk", useX)
	}()
	useX = true
	return nil
}

func main() {
	if err := run(); err != nil {
		println("!! %+v", err)
	}
}
