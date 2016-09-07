package main

import (
	"fmt"
)

func main() {
	xs := "-abc-xyz-ABC-XYZ"
	fmt.Println([]rune(xs))
	// [45 97 98 99 45 120 121 122 45 65 66 67 45 88 89 90]
}
