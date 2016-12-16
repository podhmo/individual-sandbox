package main

type Tree struct {
	Value int
	Left  *Tree
	Right *Tree
}

type L struct {
	R R
}

type R struct {
	L *L
}

type A struct {
    B B
}

type B struct {
    C C
}

type C struct {
    A A
}

type T0 struct {
	T1 T1
}

type T1 struct {
	T2 T2
}

type T2 struct{}

func main() {
}
