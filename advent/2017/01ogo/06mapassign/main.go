package main

import "fmt"

// go run -
-gcflags "-S" main.go

func main() {
	mm := map[int]int{1: 10}
	mm[2] = 20
	mm[1] = 100
	fmt.Println(mm)
}

/*
	0x009e 00158 (/home/nao/my/individual-sandbox/advent/2017/01ogo/06mapassign/main.go:9)	CALL	runtime.mapassign_fast64(SB)
*/
