package config

import (
	"encoding/json"
	"os"
)

// Config ...
type Config struct {
	LogLevel string `json:"logLevel"`
}

// NewConfig ...
func NewConfig(filename string) (*Config, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	var c Config
	decoder := json.NewDecoder(f)
	if err := decoder.Decode(&c); err != nil {
		return nil, err
	}

	return &c, nil
}
