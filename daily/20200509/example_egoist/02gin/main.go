package main

import (
	"log"

	"github.com/gin-gonic/gin"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	// Creates a gin router with default middleware:
	// logger and recovery (crash-free) middleware
	router := gin.Default()

	router.POST("/api/articles", func(c *gin.Context) {
		var input struct {
			Title   string `form:"title" binding:"required" json:"title"`
			Content string `form:"content" binding:"required" json:"content"`
		}
		// Or ShouldBindJSON
		if err := c.BindJSON(&input); err != nil {
			c.JSON(400, gin.H{"error": err.Error()})
			return
		}
		c.JSON(200, input)
	})

	// By default it serves on :8080 unless a
	// PORT environment variable was defi
	router.Run()
	return nil
}
