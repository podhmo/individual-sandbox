package main

import (
	"encoding/json"
	"fmt"
	"os"
	"time"
)

type S struct {
	Duration time.Duration `json:"duration"`
}

func main() {
	ob := &S{Duration: 1 * time.Second}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	fmt.Println(enc.Encode(ob))
}
