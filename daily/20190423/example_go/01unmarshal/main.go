package main

import (
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
)

type Config struct {
	// Services map[string]ServiceConfig `json:"services"`
	Services []ServiceConfig `json:"services"`
}

type ServiceConfig struct {
	Prefix string `json:"prefix"`
	Name   string `json:"name"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	config := `
{
  "services": [
    {"prefix": "xxx", "name": "foo"},
    {"prefix": "yyy", "name": "foo"}
  ]
}
`
	var c Config
	if err := json.Unmarshal([]byte(config), &c); err != nil {
		return err
	}
	pp.Println(c)
	return nil
}
