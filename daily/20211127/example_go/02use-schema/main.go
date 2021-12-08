package main

import (
	"encoding/json"
	"m/tutorial"
	"os"
)

func main() {
	ob := tutorial.Author{
		ID:   1,
		Name: "Foo",
	}
	json.NewEncoder(os.Stdout).Encode(ob)
}
