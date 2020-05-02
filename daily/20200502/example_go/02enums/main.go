package main

import "fmt"

type Op uint8

const (
	OpAdd Op = iota + 1
	OpSub
	OpMul
)

var (
	_ops = [...]string{"", "Add", "Sub", "Mul"}
)

func (v Op) String() string {
	return _ops[v]
}

func main() {
	fmt.Println(OpAdd)
	fmt.Println(OpAdd == OpAdd, OpAdd == OpMul)
}
