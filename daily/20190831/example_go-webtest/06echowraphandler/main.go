package main

import (
	"fmt"
	"net/http"

	"github.com/labstack/echo"
	"github.com/labstack/echo/middleware"
)

func main() {
	e := echo.New()

	e.Use(middleware.Logger())
	e.Use(middleware.Recover())

	e.GET("/", echo.WrapHandler(http.HandlerFunc(hello)))

	e.Logger.Fatal(e.Start(":8080"))
}

func hello(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintln(w, "hello")
}
