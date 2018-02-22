package main

import (
	"github.com/k0kubun/pp"
	toml "github.com/pelletier/go-toml"
)

// Postgres :
type Postgres struct {
	User     string
	Password string
}

// Config :
type Config struct {
	Postgres Postgres
}

func main() {
	doc := []byte(`
[postgres]
User = "pelletier"
Password = "mypassword"`)

	config := Config{}
	toml.Unmarshal(doc, &config)

	pp.ColoringEnabled = false
	pp.Println("user=", config)
}
