package main

import (
	"encoding/json"
	"fmt"
	"log"

	"github.com/k0kubun/pp"
)

func main() {
	{
		var toadd int64
		v := json.RawMessage([]byte("0"))
		if err := json.Unmarshal(v, &toadd); err != nil {
			pp.Println(v, err)
			log.Fatal(err)
		}
		fmt.Println(toadd)
	}
	fmt.Println("----------------------------------------")

	{
		var toadd *int64
		v := json.RawMessage([]byte("0"))
		if err := json.Unmarshal(v, toadd); err != nil {
			pp.Println(v, err)
			log.Fatal(err)
		}
	}
}
