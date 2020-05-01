package config

import (
	"encoding/json"
	"os"

	"github.com/pkg/errors"
)

type Config struct {
	Slack struct {
		APIToken string `json:"api-token"`
	} `json:"slack"`
}

func LoadConfig(filename string) (*Config, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, errors.Wrap(err, "open config")
	}

	var c Config
	decoder := json.NewDecoder(f)
	if err = decoder.Decode(&c); err != nil {
		return nil, errors.Wrap(err, "decode config")
	}

	return &c, nil
}
