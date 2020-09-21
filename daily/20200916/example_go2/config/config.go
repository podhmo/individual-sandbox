package config

import (
	"fmt"
	"os"
	"strconv"
)

type Config struct {
	Port string
	Log  LogConfig
}

type LogConfig struct {
	Name string
	JSON bool
}

func (c Config) Help() {
	fmt.Fprintln(os.Stderr, "envvars")
	fmt.Fprintln(os.Stderr, " ", "PORT (default=:50051)")
	fmt.Fprintln(os.Stderr, " ", "NAME (default=app)")
}

func New() Config {
	port := os.Getenv("PORT")
	if port == "" {
		port = ":50051"
	}

	name := os.Getenv("NAME")
	if name == "" {
		name = "app"
	}

	nojson, _ := strconv.ParseBool(os.Getenv("NOJSON"))
	usejson := !nojson

	return Config{
		Port: port,
		Log: LogConfig{
			Name: name,
			JSON: usejson,
		},
	}
}
