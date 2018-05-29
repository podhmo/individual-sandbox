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

// New :
func New(opts ...func(*App)) *App {
	app := App{
		host:  "localhost",
		port:  8888,
		debug: false,
	}

	for _, opt := range opts {
		opt(&app)
	}

	// 何かconnectionをつなげたりだとかdefault値として最初に代入しておきたくないものがある場合にはこう書く
	// if app.xxx == nil {
	//     app.xxx = &newXXX()
	// }
	return &app
}

// WithHost :
func WithHost(host string) func(*App) {
	return func(app *App) {
		app.host = host
	}
}

// WithPort :
func WithPort(port int) func(*App) {
	return func(app *App) {
		app.port = port
	}
}

// WithDebug :
func WithDebug(debug bool) func(*App) {
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
