package main

import "fmt"

// S :
type S struct{}

func main() {
	var s *S
	var i interface{}
	i = s

	// これだとnilを対象にしたときにも取得してしまう。
	if ob, ok := i.(*S); ok {
		fmt.Println(ob)
	}

	// 実はokを無視して取得できた値がnilでないか調べればよかった(失敗時にpanicにならないようにシャドーイングはいる)。
	if ob, _ := i.(*S); ob != nil {
		fmt.Println(ob)
	}
}
