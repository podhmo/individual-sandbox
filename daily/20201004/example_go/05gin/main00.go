package main

import (
	"os"

	"github.com/gin-gonic/gin"
)

func main() {
	r := gin.Default()
	r.GET("/ping", func(c *gin.Context) {
		c.JSON(200, gin.H{
			"message": "pong",
		})
	})

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":8080"
	}
	r.Run(addr) // listen and serve on 0.0.0.0:8080 (for windows "localhost:8080")
}
