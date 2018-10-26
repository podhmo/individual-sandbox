package main

import "fmt"

func main() {
	k0 := []string{"foo", "bar", "boo"}
	k1 := []string{"foo", "bar", "boo"}

	m := map[[]string]int{} // invalid map key type
	m[k0] += 1
	m[k1] += 1

	fmt.Println(m)

    x := 1
    use(x)
}
