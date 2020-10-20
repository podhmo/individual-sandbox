package main

import (
	"fmt"
	_ "unsafe"
)

//go:linkname time_now time.now
func time_now() (sec int64, nsec int32)

func main() {
	sec, nsec := time_now()
	fmt.Println(sec, nsec)
}
