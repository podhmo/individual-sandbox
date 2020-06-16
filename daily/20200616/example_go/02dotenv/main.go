package main

import (
	"fmt"
	"log"

	"m/02dotenv/config"

	"github.com/joho/godotenv"
)

// from: https://dev.to/craicoverflow/a-no-nonsense-guide-to-environment-variables-in-go-a2f

func main() {
	// loads values from .env into the system
	if err := godotenv.Load(); err != nil {
		log.Print("No .env file found")
	}

	conf := config.New()

	// Print out environment variables
	fmt.Println(conf.GitHub.Username)
	fmt.Println(conf.GitHub.APIKey)
	fmt.Println(conf.DebugMode)
	fmt.Println(conf.MaxUsers)

	// Print out each role
	for _, role := range conf.UserRoles {
		fmt.Println("	", role)
	}
}
