package main

import (
	"os"

	"github.com/gofiber/fiber"
	"github.com/gofiber/fiber/middleware"
)

func main() {
	app := fiber.New()

	// Pass a custom output
	app.Use(middleware.Logger(os.Stdout))

	// Default RequestID
	app.Use(middleware.RequestID())

	app.Get("/", func(c *fiber.Ctx) {
		c.Send("Hello, World 👋!")
	})

	app.Listen(3000)
}
