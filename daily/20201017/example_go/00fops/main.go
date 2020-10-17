package main

type Config struct {
	Port     int
	MaxConns int
}

func NewServer(host string, options ...func(*Config)) *Server {
	c := &Config{
		Port:     8080,
		MaxConns: 1,
	}
	for _, opt := range options {
		opt(c)
	}
	return newServer(host, c)
}
