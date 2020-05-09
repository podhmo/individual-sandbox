package main

import (
	"fmt"
	"log"
	"os"

	"github.com/go-playground/validator"
	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

// https://echo.labstack.com/guide/request#json-data
// Echo doesn’t have built-in data validation capabilities, however, you can register a custom validator using Echo#Validator and leverage third-party libraries.

type (
	Article struct {
		Title   string `json:"title" validate:"required"`
		Content string `json:"content" validate:"required"`
	}

	CustomValidator struct {
		validator *validator.Validate
	}
)

func (cv *CustomValidator) Validate(i interface{}) error {
	return cv.validator.Struct(i)
}

func run() error {
	e := echo.New()

	// Middleware
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())

	// https://echo.labstack.com/guide/request#validate-data
	e.Validator = &CustomValidator{validator: validator.New()}

	e.POST("/api/articles", func(c echo.Context) error {
		var ob Article
		if err := c.Bind(&ob); err != nil {
			return err
		}
		if err := c.Validate(&ob); err != nil {
			// if just return err, `500 {"message": "Internal Server Error"}`
			return echo.NewHTTPError(400, err.Error())
		}
		return c.JSON(200, ob)

	})

	// Start server
	port := os.Getenv("PORT")
	return e.Start(fmt.Sprintf(":%s", port))
}
