package main

import (
	"encoding/json"
	"os"
)

func main() {
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")

	{
		var xs []int
		encoder.Encode(xs)
	}
	{
		var xs []string
		encoder.Encode(xs)
	}
}
