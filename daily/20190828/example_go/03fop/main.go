package main

import "github.com/k0kubun/pp"

// Server :
type Server struct {
	Host string
	Port int
}

// Option :
type Option interface {
	Apply(*Server)
}

type withHost struct{ Host string }

func (w withHost) Apply(s *Server) { s.Host = w.Host }

// Host :
func Host(host string) interface{ Option } {
	return withHost{Host: host}
}

type withPort struct{ Port int }

func (w withPort) Apply(s *Server) { s.Port = w.Port }

// Port :
func Port(port int) interface{ Option } {
	return withPort{Port: port}
}

// New :
func New(options ...Option) *Server {
	s := &Server{
		Host: "localhost",
		Port: 8080,
	}
	for _, opt := range options {
		opt.Apply(s)
	}
	return s
}

func main() {
	pp.Println(New())
	pp.Println(New(
		Host("xxx"),
		Port(44444),
	))
}
