package main

import (
	"github.com/labstack/echo"
	"github.com/labstack/echo/middleware"
)

func main() {
	e := echo.New()

	e.Use(middleware.Logger())
	e.Use(middleware.Recover())

	e.GET("/", hello)

	e.Logger.Fatal(e.Start(":8080"))
}

func hello(c echo.Context) error {
	// return fmt.Errorf("hello")
	return echo.NewHTTPError(400, "bad")
}
