package main

type Config struct {
	Host string
	Port int
}
type Server struct {
	*Config
	Default *Config
}

func New() *Server {
	Default := &Config{Host: "example.net", Port: 8080}
	return &Server{
		Config: Default,
	}
}
