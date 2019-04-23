package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strings"

	"github.com/k0kubun/pp"
)

type Config struct {
	Services ServiceConfigMap `json:"services"`
}

type ServiceConfigMap struct {
	defaultValue ServiceConfig
	values       map[string]ServiceConfig
}

// Get :
func (m *ServiceConfigMap) Get(name string) ServiceConfig {
	v, ok := m.values[name]
	if !ok {
		return m.defaultValue
	}
	return v
}

func (m *ServiceConfigMap) UnmarshalJSON(data []byte) error {
	var services []ServiceConfig
	if !strings.HasPrefix(strings.TrimLeft(string(data), "\n \t"), "[") {
		var s ServiceConfig
		if err := json.Unmarshal(data, &s); err != nil {
			return err
		}
		m.defaultValue = s
		services = append(services, s)
	} else {
		// TODO: len() == 0
		if err := json.Unmarshal(data, &services); err != nil {
			return err
		}
		m.defaultValue = services[0]
	}

	if m.values == nil {
		m.values = map[string]ServiceConfig{}
	}
	for _, s := range services {
		m.values[s.Prefix] = s
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
	{
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
		pp.Println(c.Services.Get("xxx"))
		pp.Println(c.Services.Get("zzz"))

	}
	fmt.Println("----------------------------------------")
	{
		config := `
{
  "services":
    {"prefix": "xxx", "name": "foo"}
}
`
		var c Config
		if err := json.Unmarshal([]byte(config), &c); err != nil {
			return err
		}
		pp.Println(c)
		pp.Println(c.Services.Get("xxx"))
		pp.Println(c.Services.Get("zzz"))

	}
	return nil
}
