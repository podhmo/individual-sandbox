package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"

	"github.com/hashicorp/hcl"
	"github.com/k0kubun/pp"
)

func main() {
	for _, filename := range os.Args[1:] {
		fmt.Fprintln(os.Stdout, "----------------------------------------")
		fmt.Fprintln(os.Stdout, filename)
		fmt.Fprintln(os.Stdout, "----------------------------------------")
		d, err := ioutil.ReadFile(filename)
		if err != nil {
			log.Printf("err: %s", err)
			continue
		}

		fmt.Println(string(d))
		var out interface{}
		if err := hcl.Unmarshal(d, &out); err != nil {
			log.Printf("decode error: %s", err)
			continue
		}

		pp.ColoringEnabled = false
		pp.Println(out)
	}
}
