package main

import (
	"encoding/json"
	"log"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

// M :
type M map[string]interface{}

func run() error {
	m := M{
		"x0": 10,
		"x1": 10,
		"y0": M{"v": 10},
		"y1": M{"v": 10},
		"y2": M{"v": 10},
		"y3": M{"v": 10},
	}
	var copied M
	copy(copied, m)                // compile error
	copied["x1"] = 20              // update
	copied["y1"] = M{"v": 20}      // update
	copied["y2"].(M)["v"] = 20     // update
	copied["y3"].(M)["extra"] = 20 // insert

	encoder := json.NewEncoder(os.Stdout)
	encoder.Encode(m)
	encoder.Encode(copied)
	return nil
}
