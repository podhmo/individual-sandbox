package m

import "fmt"

type Getter interface {
	Get(key string) string
}

func PrintIfGet(g Getter, k string) {
	v := g.Get(k)
	if v != "" {
		fmt.Println("yay", k, v)
	}
}
