package myapitest

import (
	"myapi/myapitest/forrecorder"
	"myapi/myapitest/interfaces"
	"net/http"
)

// Client :
type Client = interfaces.Client

// Response :
type Response = interfaces.Response

// NewClientForRecorder :
func NewClientForRecorder(handlerFunc http.HandlerFunc, options ...func(*Config)) Client {
	c := &Config{}
	for _, opt := range options {
		opt(c)
	}
	return &forrecorder.Client{
		HandlerFunc: handlerFunc,
		BasePath:    c.BasePath,
	}
}

// WithBasePath :
func WithBasePath(basePath string) func(*Config) {
	return func(c *Config) {
		c.BasePath = basePath
	}
}

// Config :
type Config struct {
	BasePath string
}
