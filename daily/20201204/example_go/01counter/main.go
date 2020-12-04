package main

func main() {
	m := map[[]int]int{}
	m[[]int{1}] = 1 // invalid map key
	m[[]int{1, 2}] = 2
}
