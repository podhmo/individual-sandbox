package main

import (
	"log"
	"os"

	"github.com/comail/colog"
)

// https://github.com/comail/colog

func main() {
	colog.Register()
	colog.SetOutput(os.Stdout)
	colog.ParseFields(true)
	colog.SetFlags(log.Ldate | log.Lshortfile)

	log.Print("debug: logging this to json")
	log.Print("info: logging this to json")
	log.Print("warning: with fields foo=bar")
	log.Print("error: with fields foo=bar")
	log.Print("alert: with fields foo=bar")
}
