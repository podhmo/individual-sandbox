package main

import (
	"log"

	"github.com/k0kubun/pp"
	"github.com/podhmo/commithistory/config"
)

// Config :
type Config struct {
	Message string `json:"message"`
}

func main() {
	c := config.New("foo")
	var conf Config
	if err := c.Load("config.json", &conf); err != nil {
		log.Fatal(err)
	}

	pp.Println(conf)

	conf.Message = "hello"
	c.Save("config.json", &conf)
}
