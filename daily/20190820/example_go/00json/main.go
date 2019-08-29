package main

import (
	"encoding/json"
	"os"
)

type P struct {
	*Q
}
type Q struct {
	X int `json:"x" bson:"x"`
	Y int `json:"y" bson:"y"`
}

func main() {
	p := &P{Q: &Q{X: 10, Y: 20}}
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	encoder.Encode(p)
}
