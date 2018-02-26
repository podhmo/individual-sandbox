package main

import (
	"encoding/json"
	"fmt"

	"github.com/k0kubun/pp"
)

// User :
type User struct {
	Name string `required:"true"`
	Age  int    `required:"true"`
}

func main() {
	pp.ColoringEnabled = false
	{
		var u User
		pp.Println(json.Unmarshal([]byte(`{}`), &u))
		pp.Println(u)
	}
	fmt.Println("----------------------------------------")
	{
		var u User
		pp.Println(json.Unmarshal([]byte(`{"name": "","age": 0}`), &u))
		pp.Println(u)
	}
	fmt.Println("----------------------------------------")
	{
		var u User
		pp.Println(json.Unmarshal([]byte(`{"name": "a", "age": 0}`), &u))
		pp.Println(u)
	}
	fmt.Println("----------------------------------------")
	{
		var u User
		pp.Println(json.Unmarshal([]byte(`{"name": "", "age": 1}`), &u))
		pp.Println(u)
	}
}
