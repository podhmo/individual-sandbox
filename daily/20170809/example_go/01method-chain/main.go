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
func NewConfig() *Config {
	config := Config{
		DB: DB{
			URL:  "http://localhost:8888",
			Name: "testdb",
		},
		LogLevel: LogLevelInfo,
	}
	return &config
}

// WithDB :
func (c *Config) WithDB(url string, name string) *Config {
	c.DB.URL = url
	c.DB.Name = name
	return c
}

// WithLogLevel :
func (c *Config) WithLogLevel(level LogLevel) *Config {
	c.LogLevel = level
	return c
}

func main() {
	fmt.Println("default")
	pp.Println(NewConfig())

	fmt.Println("custom")
	pp.Println(NewConfig().WithDB("http://me.com/db", "testdb").WithLogLevel(LogLevelDebug))
}
