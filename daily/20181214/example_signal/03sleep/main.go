package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("sleep", 2)
	time.Sleep(2 * time.Second)
	fmt.Println("end")
}
