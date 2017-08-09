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

// SetDB :
func SetDB(url string, name string) func(*Config, error) (*Config, error) {
	return func(c *Config, err error) (*Config, error) {
		if err != nil {
			return c, err
		}
		c.DB.URL = url
		c.DB.Name = name
		return c, nil
	}
}

// SetLogLevel :
func SetLogLevel(level LogLevel) func(*Config, error) (*Config, error) {
	return func(c *Config, err error) (*Config, error) {
		if err != nil {
			return c, err
		}
		c.LogLevel = level
		return c, nil
	}
}

// Lift :
func Lift(c *Config) (*Config, error) {
	return c, nil
}

func main() {
	config := Config{
		DB: DB{
			URL:  "http://localhost:8888",
			Name: "testdb",
		},
		LogLevel: LogLevelInfo,
	}

	fmt.Println("default")
	pp.Println(config)

	fmt.Println("custom")
	config2, err := SetLogLevel(LogLevelDebug)(SetDB("http://me.com/db", "testdb")(Lift(&config)))
	if err != nil {
		log.Fatal(err)
	}
	pp.Println(config2)
}
