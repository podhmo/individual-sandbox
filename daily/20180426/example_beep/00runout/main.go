package main

import (
	"fmt"
	"log"

	"github.com/gen2brain/beeep"
)

func main() {
	for i := 0; i < 1000; i++ {
		log.Println(i)
		msg := fmt.Sprintf("ping %d", i)
		if err := beeep.Notify(msg, msg, ""); err != nil {
			log.Fatal(err)
		}
	}
}
