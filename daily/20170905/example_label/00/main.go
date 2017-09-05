package main

import "fmt"

func main() {
LOOP:
	for i := 0; i < 5; i++ {
		fmt.Println("start", i)
		for j := 0; j < 5; j++ {
			fmt.Println("  start", i, j)
			if j == 3 {
				continue LOOP
			}
			fmt.Println("  end  ", i, j)
		}
		fmt.Println("end  ", i)
	}
}
