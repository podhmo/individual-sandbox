package main

import (
	"time"

	"github.com/podhmo/ctxlog/stdctxlog"
)

func main() {
	log := stdctxlog.New()
	log.With("now", time.Now()).Info("hai")
}
