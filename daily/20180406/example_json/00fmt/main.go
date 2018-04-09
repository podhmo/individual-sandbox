package main

import (
	"encoding/json"
	"fmt"
	"log"
)

func main() {
	b, err := json.Marshal(fmt.Errorf("%s hai", "foo"))
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("@", string(b), "@")
}
