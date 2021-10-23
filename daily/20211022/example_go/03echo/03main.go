package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
)

type Article struct {
	Title string `json:"title"`
}

var articles = map[int64]*Article{
	1: {Title: "hello"},
	2: {Title: "byebye"},
}

func ListArticle(c echo.Context) error {
	result := make([]*Article, 0, len(articles))
	for _, ob := range articles {
		result = append(result, ob)
	}
	return c.JSON(http.StatusOK, result)
}

type GetArticleInput struct {
	ID int64 `param:"articleId"`
}

func GetArticle(c echo.Context) error {
	var input GetArticleInput
	if err := c.Bind(&input); err != nil {
		return err
	}

	ob, ok := articles[input.ID]
	if !ok {
		return c.JSON(http.StatusOK, map[string]string{"message": "not found"})
	}
	return c.JSON(http.StatusOK, ob)
}

func main() {
	port := 8888
	if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
		port = v
	}
	log.Print("listen ...", port)

	// Echo instance
	e := echo.New()

	// Middleware
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())

	// Routes
	e.GET("/articles", ListArticle)
	e.GET("/articles/:articleId", GetArticle)

	// Start server
	addr := fmt.Sprintf(":%d", port)
	if err := e.Start(addr); err != nil {
		e.Logger.Fatalf("!! %+v", err)
	}
}
