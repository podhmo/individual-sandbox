package main

import (
	"log"
	"time"

	"go.uber.org/zap"
)

func main() {
	logger, err := zap.NewProduction()
	if err != nil {
		log.Fatal(err)
	}
	defer logger.Sync() // flushes buffer, if any

	sugar := logger.Sugar()
	sugar.Infow("failed to fetch URL",
		"url", "http://example.net",
		"attempt", 3,
		"backof", time.Second,
	)
	sugar.Infof("Failed to fetch URL: %q", "http://example.net")
}
