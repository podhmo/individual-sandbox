package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"os"
)

type Shape struct {
	Type      ShapeType  `json:"$type"`
	Rectangle *Rectangle `json:"rectangle,omitempty"`
	Circle    *Circle    `json:"circle,omitempty"`
}

type ShapeType string

const (
	ShapeTypeRectangle ShapeType = "Rectangle"
	ShapeTypeCircle              = "Circle"
)

type Rectangle struct {
	Width  float64 `json:"width"`
	Height float64 `json:"height"`
}

func (c *Rectangle) Shape() *Shape {
	return &Shape{
		Type:      ShapeTypeRectangle,
		Rectangle: c,
	}
}

type Circle struct {
	Radius float64 `json:"radius"`
}

func (c *Circle) Shape() *Shape {
	return &Shape{
		Type:   ShapeTypeCircle,
		Circle: c,
	}
}
func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	{
		s := &Shape{Type: ShapeTypeCircle, Circle: &Circle{Radius: 3.2}}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(s); err != nil {
			return err
		}
	}

	{
		var s Shape
		dec := json.NewDecoder(bytes.NewBufferString(`
{
  "$type": "Circle",
  "circle": {
    "radius": 3.2
  }
}
	`))
		if err := dec.Decode(&s); err != nil {
			return err
		}
		fmt.Printf("%#+v\n", s)
	}
	return nil
}
