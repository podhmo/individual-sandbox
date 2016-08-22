package main

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
)

func calc(id int, n int) {
	fmt.Printf("%2d%s start: (%d)\n", id, strings.Repeat(" ", id), n)
	time.Sleep(time.Duration(n) * time.Millisecond)
	fmt.Printf("%2d%s end:\n", id, strings.Repeat(" ", id))
}

func main() {
	calc(1, 200)
	calc(2, rand.Intn(2000))
}
