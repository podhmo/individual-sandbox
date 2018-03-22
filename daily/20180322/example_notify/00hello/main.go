package main

import (
	"log"

	"github.com/gen2brain/beeep"
)

func main() {
	log.Println("start")
	if err := beeep.Alert("gen2brain/beep", "hello!!", "assets/warning.png"); err != nil {
		log.Fatal(err)
	}
	if err := beeep.Notify("gen2brain/beep", "hello", "assets/information.png"); err != nil {
		log.Fatal(err)
	}
	log.Println("end")
}
