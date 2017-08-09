package main

import (
	"fmt"

	"github.com/k0kubun/pp"
)

// LogLevel :
type LogLevel uint8

// LogLevel :
const (
	LogLevelDebug LogLevel = iota
	LogLevelInfo
	LogLevelWarning
	LogLevelError
)

// Config :
type Config struct {
	DB       DB
	LogLevel LogLevel
}

// DB :
type DB struct {
	URL  string
	Name string
}

// NewConfig :
func NewConfig(modifies ...func(*Config)) *Config {
	config := Config{
		DB: DB{
			URL:  "http://localhost:8888",
			Name: "testdb",
		},
		LogLevel: LogLevelInfo,
	}
	for _, modify := range modifies {
		modify(&config)
	}
	return &config
}

// SetDB :
func SetDB(url string, name string) func(c *Config) {
	return func(c *Config) {
		c.DB.URL = url
		c.DB.Name = name
	}
}

// SetLogLevel :
func SetLogLevel(level LogLevel) func(c *Config) {
	return func(c *Config) {
		c.LogLevel = level
	}
}

func main() {
	fmt.Println("default")
	pp.Println(NewConfig())

	fmt.Println("custom")
	pp.Println(NewConfig(SetDB("http://me.com/db", "testdb"), SetLogLevel(LogLevelDebug)))
}
