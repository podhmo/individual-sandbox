package main

import (
	"fmt"
	"log"

	"github.com/caarlos0/env/v6"
	"github.com/joho/godotenv"
)

type GitHubConfig struct {
	Username string `env:"GITHUB_USERNAME"`
	APIKey   string `env:"GITHUB_API_KEY"`
}

type Config struct {
	GitHub    GitHubConfig
	DebugMode bool     `env:"DEBUG_MODE"`
	UserRoles []string `env:"USER_ROLES"`
	MaxUsers  int      `env:"MAX_USERS"`
}

func main() {
	// loads values from .env into the system
	if err := godotenv.Load(); err != nil {
		log.Print("No .env file found")
	}

	var conf Config
	if err := env.Parse(&conf); err != nil {
		panic(err)
	}

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
