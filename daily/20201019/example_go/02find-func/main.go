package main

import (
	"fmt"
	_ "text/template"
)

// TODO
// http://www.alangpierce.com/blog/2016/03/17/adventures-in-go-accessing-unexported-functions/
func main() {
	fn := FindFuncWithName("template.findFunction")
	fmt.Println(fn)
}
