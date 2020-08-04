package main

import (
	"log"

	"github.com/comail/colog"
)

func main() {
	colog.Register()
	log.Print("info: that's all it takes!")
}
