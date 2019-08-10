package main

import (
	"log"

	"go.uber.org/zap"
)

// https://godoc.org/go.uber.org/zap#ex-NewStdLog
// https://godoc.org/log

func main() {
	logger := zap.NewExample()
	defer logger.Sync()

	std := zap.NewStdLog(logger)
	std.SetFlags(log.LstdFlags | log.Lshortfile)
	std.Print("standard logger wrapper")
}
