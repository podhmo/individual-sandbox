package main

import "github.com/k0kubun/pp"

// Server :
type Server struct {
	Host string
	Port int
}

// Host :
func Host(host string) func(*Server) {
	return func(s *Server) {
		s.Host = host
	}
}

// Port :
func Port(port int) func(*Server) {
	return func(s *Server) {
		s.Port = port
	}
}

// New :
func New(options ...func(*Server)) *Server {
	s := &Server{
		Host: "localhost",
		Port: 8080,
	}
	for _, opt := range options {
		opt(s)
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
