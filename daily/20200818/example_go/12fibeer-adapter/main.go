package main

import (
	"fmt"
	"net/http"

	"github.com/gofiber/adaptor"
	"github.com/gofiber/fiber"
)

func main() {
	// New fiber app
	app := fiber.New()

	// http.Handler -> func(*fiber.Ctx)
	app.Get("/", adaptor.HTTPHandler(handler(greet)))

	// http.HandlerFunc -> func(*fiber.Ctx)
	app.Get("/func", adaptor.HTTPHandlerFunc(greet))

	// Listen on port 3000
	app.Listen(3000)
}

func handler(f http.HandlerFunc) http.Handler {
	return http.HandlerFunc(f)
}

func greet(w http.ResponseWriter, r *http.Request) {
	fmt.Fprint(w, "Hello World!")
}
