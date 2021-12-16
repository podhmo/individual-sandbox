package action

import (
	"context"
	"log"
	"os"
)

type HelloOutput struct {
	Message string `json: "message"`
}

func Hello(ctx context.Context, logger *log.Logger) (*HelloOutput, error) {
	logger.Printf("hello")
	return &HelloOutput{Message: "hello"}, nil
}

func NewLogger() (*log.Logger, error) {
	return log.New(os.Stderr, "app ", 0), nil
}
