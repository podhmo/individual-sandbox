package main

import (
	"encoding/json"
	"os"
	"time"
)

func main() {
	now := time.Now()
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	encoder.Encode(now)
}
