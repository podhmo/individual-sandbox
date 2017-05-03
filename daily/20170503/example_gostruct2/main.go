package main

import (
	"encoding/json"
	"log"
	"os"
	"github.com/k0kubun/pp"
)

func main() {
	decoder := json.NewDecoder(os.Stdin)
	var conf Conf
	if err := decoder.Decode(&conf); err != nil {
		log.Fatal(err)
	}
	pp.Print(conf)
}
