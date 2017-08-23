package main

import (
	"fmt"
	"os"
	"runtime/pprof"
	"time"
)

// GOGCTRACE=1
func main() {
	var arr [][]int

	var heapp *pprof.Profile
	for _, p := range pprof.Profiles() {
		if p.Name() == "heap" {
			heapp = p
			break
		}
	}
	if heapp == nil {
		panic("hmm")
	}

	for i := 0; i < 10; i++ {
		fmt.Println(i)
		heapp.WriteTo(os.Stdout, 1)
		arr = append(arr, make([]int, 100000))
		time.Sleep(500 * time.Millisecond)
	}
	fmt.Println(".")
}
