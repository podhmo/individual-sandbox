package main

import "fmt"

func main() {
	xs := []string{"x", "y", "z"}
	ys := []string{"a", "b", "c"}
	{
		fmt.Println("cross2")
		fmt.Println(len(cross2(xs, ys)))
		fmt.Println(cross2(xs, ys))
	}
	{
		zs := []string{"i", "j", "k"}
		as := []string{"1", "2", "3"}
		bs := []string{"#", "$", "%"}
		fmt.Println("cross5")
		fmt.Println(len(cross5(xs, ys, zs, as, bs)))
		fmt.Println(cross5(xs, ys, zs, as, bs))
	}
	{
		for i := 1; i < 20; i++ {
			x, _ := Fizzbuzz(i)
			fmt.Printf("%s ", x)
		}
	}
}
