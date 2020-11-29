package main

import (
	"errors"
	"fmt"
	"os"
)

func main() {
	f, err := os.Open("xxx")
	if err != nil {
		fmt.Println(err == os.ErrNotExist, errors.Is(err, os.ErrNotExist))
		fmt.Printf("open %+v\n", err)
	}
	defer f.Close()
}
