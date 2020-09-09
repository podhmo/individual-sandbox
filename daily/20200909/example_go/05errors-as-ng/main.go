package main

import (
	"errors"
	"fmt"
	"os"
)

type hasTimeout interface {
	Timeout() bool
}

func main() {
	if _, err := os.Open("non-existing"); err != nil {
		var pathError hasTimeout
		if errors.As(err, pathError) {
			fmt.Println("Failed at path:", pathError, "!!", pathError.Timeout())
		} else {
			fmt.Println(err)
		}
	}
}
