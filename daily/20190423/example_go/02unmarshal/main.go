package main

import (
	"encoding/json"
	"log"

	"github.com/k0kubun/pp"
)

type Config struct {
	Services ServiceConfigMap `json:"services"`
}

type ServiceConfigMap struct {
	M map[string]ServiceConfig
}

func (m *ServiceConfigMap) UnmarshalJSON(data []byte) error {
	var services []ServiceConfig
	if err := json.Unmarshal(data, &services); err != nil {
		return err
	}

	if m.M == nil {
		m.M = map[string]ServiceConfig{}
	}
	for _, s := range services {
		m.M[s.Prefix] = s
	}
	return nil
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
	pp.Println(c.Services.M["xxx"])
	return nil
}
