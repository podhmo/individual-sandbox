package main

import (
	"log"
	"m/config"
	"m/web"
	"net/http"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	c := config.New()
	for _, x := range os.Args {
		if x == "-h" {
			c.Help()
			return nil
		}
		if x == "--help" {
			c.Help()
			return nil
		}
	}

	log.Println("listening ...", c.Port)
	r := web.NewServerFromConfig(c)
	return http.ListenAndServe(c.Port, r)
}
