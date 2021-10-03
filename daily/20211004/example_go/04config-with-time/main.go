package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/k0kubun/pp"
)

type Config struct {
	CreatedAt time.Time `json:"createdAt"`
	config
}

func (c *Config) UnmarshalJSON(b []byte) error {
	if err := json.Unmarshal(b, &c.config); err != nil {
		return err
	}
	v, err := time.Parse(time.RFC3339, c.config.CreatedAt)
	if err != nil {
		return fmt.Errorf("Config.CreatedAt: %w", err)
	}

	v = v.Add(time.Second * 60 * 60 * 24 * 500) // unmarshalJSONに対応していないfieldを模す (configはdumpできなくて良いので)
	c.CreatedAt = v
	return nil
}

type config struct {
	CreatedAt string `json:"createdAt"`
	Name      string `json:"name"`
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	var c Config
	if err := json.Unmarshal(
		[]byte(`{"name": "foo", "createdAt": "2000-01-01T00:00:00Z"}`),
		&c,
	); err != nil {
		return err
	}
	pp.ColoringEnabled = false
	pp.Println(c)
	return json.NewEncoder(os.Stdout).Encode(c)
}
