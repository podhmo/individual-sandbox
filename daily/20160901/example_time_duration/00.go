package main

import (
	"fmt"
	"time"
)

func main() {
    {
        // 良くない
        t := time.Duration(30) * time.Second
        fmt.Printf("%[1]T: %[1]v\n", t)

    }
    {
        // 良い
        t := 30 * time.Second
        fmt.Printf("%[1]T: %[1]v\n", t)

    }
}
