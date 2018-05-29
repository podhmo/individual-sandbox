package main

import (
	"log"

	"github.com/k0kubun/pp"
)

// App :
type App struct {
	host  string
	port  int
	debug bool
}

// Option :
type Option func(*App)

// New :
func New(opts ...Option) *App {
	app := App{
		host:  "localhost",
		port:  8888,
		debug: false,
	}
	for _, opt := range opts {
		opt(&app)
	}
	return &app
}

// WithHost :
func WithHost(host string) Option {
	return func(app *App) {
		app.host = host
	}
}

// WithPort :
func WithPort(port int) Option {
	return func(app *App) {
		app.port = port
	}
}

// WithDebug :
func WithDebug(debug bool) Option {
	return func(app *App) {
		app.debug = debug
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	app := New(WithDebug(true))
	pp.Println(app)
	return nil
}
