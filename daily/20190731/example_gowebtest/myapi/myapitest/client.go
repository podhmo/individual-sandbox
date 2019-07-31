package myapitest

import (
	"myapi/myapitest/forrecorder"
	"myapi/myapitest/forserver"
	"myapi/myapitest/interfaces"
	"net/http"
	"net/http/httptest"
)

// Client :
type Client = interfaces.Client

// Response :
type Response = interfaces.Response

// NewClientForServer :
func NewClientForServer(ts *httptest.Server, options ...func(*Config)) Client {
	c := &Config{}
	for _, opt := range options {
		opt(c)
	}
	return &forserver.Client{
		Server:   ts,
		BasePath: c.BasePath,
	}
}

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
