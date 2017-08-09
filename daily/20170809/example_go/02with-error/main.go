package main

import (
	"fmt"
	"log"

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

// RunConfig :
func RunConfig(config *Config, actions ...func(*Config) error) (*Config, error) {
	for _, action := range actions {
		if err := action(config); err != nil {
			return nil, err
		}
	}
	return config, nil
}

// Lift :
func Lift(modify func(c *Config)) func(c *Config) error {
	return func(c *Config) error {
		modify(c)
		return nil
	}
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
	config, err := RunConfig(
		NewConfig(),
		Lift(SetDB("http://me.com/db", "testdb")),
		Lift(SetLogLevel(LogLevelDebug)),
	)
	if err != nil {
		log.Fatal(err)
	}
	pp.Println(config)
}
