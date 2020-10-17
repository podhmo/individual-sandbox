package main

type Config struct {
	Port     int
	MaxConns int
}

func (c *Config) Apply(target *Config) {
	target.Port = c.Port
	target.MaxConns = c.MaxConns
}

type ServerOpt interface {
	Apply(*Config)
}

type ServerOptFunc func(*Config)

func (f ServerOptFunc) Apply(c *Config) {
	f(c)
}
func WithPort(port int) ServerOpt {
	return ServerOptFunc(func(c *Config) {
		c.Port = port
	})
}
func NewServer(host string, options ...ServerOpt) *Server {
	c := &Config{
		Port:     8080,
		MaxConns: 1,
	}
	for _, opt := range options {
		opt(c)
	}
	return &Server{
		Host:     host,
		Port:     c.Port,
		MaxConns: c.MaxConns,
	}
}

type Server struct {
	Host     string
	Port     int
	MaxConns int
}
