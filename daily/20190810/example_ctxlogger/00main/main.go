package main

import (
	"time"

	"github.com/podhmo/ctxlog/zapctxlog"
)

func main() {
	log, _, teardown := zapctxlog.New()
	defer teardown()
	log.With("now", time.Now()).Info("hai")
}
