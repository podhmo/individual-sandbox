package database

import "os"

type Config struct {
	URL string `json:"url"`
}

type LookupFunc func(string) (string, bool)

var Lookup = os.LookupEnv

func (c *Config) LoadFromENV() {
	url, ok := Lookup("URL")
	if ok {
		c.URL = url
	}
}
