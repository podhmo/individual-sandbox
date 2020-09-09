package main

import (
	"errors"
	"fmt"
	"os"
)

type error struct {
}

func (e error) Error() string {
	return "me"
}

func main() {
	if _, err := os.Open("non-existing"); err != nil {
		var pathError *error
		if errors.As(err, &pathError) {
			fmt.Println("Failed at path:", pathError.Error())
		} else {
			fmt.Println(err)
		}
	}
}
