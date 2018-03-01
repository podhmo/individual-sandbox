package main

oimport "fmt"

func main() {
	xs := []int{1, 2, 3}
	i := 1
	ys := []int{10}
	ys = append(ys, xs[i:]...)
	fmt.Println(ys)
}
