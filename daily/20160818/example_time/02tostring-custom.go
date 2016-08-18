package main

import (
	"fmt"
	"time"
)

func main(){
    t := time.Now()
    fmt.Printf("%%s: %s\n", t)
    fmt.Printf("%%v: %v\n", t)
    fmt.Printf("%%+v: %v\n", t)

    fmt.Printf("RubyDate: %s\n", t.Format(time.RubyDate))
    fmt.Printf("RFC3339: %s\n", t.Format(time.RFC3339))
    fmt.Printf("Kitchen: %s\n", t.Format(time.Kitchen))
}
