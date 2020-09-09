package main

import (
	"errors"
	"fmt"
	"os"
)

func main() {
	if _, err := os.Open("non-existing"); err != nil {
		var pathError *os.PathError
		if errors.As(err, pathError) {
			fmt.Println("Failed at path:", pathError.Path)
		} else {
			fmt.Println(err)
		}
	}
}
