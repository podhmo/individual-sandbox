package main

import (
	"fmt"
)

func incByDefer() int{
    i := 0
    defer func(){
        i++
    }()
    return i
}

func incByDefer2() *int{
    i := 0
    result := &i
    defer func(){
        i++
    }()
    return result
}


func main(){
    fmt.Println(incByDefer()) // 0
    fmt.Println(*incByDefer2()) // 1
}
