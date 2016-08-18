package main

import (
	"fmt"
	"time"
)

func main() {
	s := "2016-08-18 19:54:52.105747747 +0900 JST"
	// "1月2日午後3時4分5秒2006年"
	layout := "2006-01-02 15:04:05.9 -0700 JST"
	t, err := time.Parse(layout, s)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%T, %+v\n", t, t)
	fmt.Println(t.UnixNano())
}
