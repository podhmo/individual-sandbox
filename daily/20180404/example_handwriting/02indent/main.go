package main

import (
	"os"

	"github.com/podhmo/handwriting/indent"
)

func main() {
	o := indent.New(os.Stdout)
    o.WithBlock("func Sum(xs []int) int", func(){
        o.Println("n := 0")
        o.WithBlock("for _, x := range xs", func(){
            o.Println("n += x")
        })
        o.Println("return n")
    })
}
