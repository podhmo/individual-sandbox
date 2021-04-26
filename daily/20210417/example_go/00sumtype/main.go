package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"os"
)

type Shape interface {
	Shape() ShapeType
}

type ShapeType string

const (
	ShapeTypeRectangle ShapeType = "Rectangle"
	ShapeTypeCircle              = "Circle"
)

type rectangle struct {
	Width  float64 `json:"width"`
	Height float64 `json:"height"`
}
type Rectangle rectangle

func (*Rectangle) Shape() ShapeType {
	return ShapeTypeRectangle
}

type circle struct {
	Radius float64 `json:"radius"`
}
type Circle circle

func (*Circle) Shape() ShapeType {
	return ShapeTypeCircle
}

func (c *Circle) MarshalJSON() ([]byte, error) {
	b, err := json.Marshal((*circle)(c))
	if err != nil {
		return nil, err
	}
	buf := bytes.NewBuffer(b)
	buf.Truncate(len(b) - 1)
	if _, err := fmt.Fprintf(buf, `, "$type": %q}`, c.Shape()); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	{
		var s Shape = &Circle{Radius: 3.2}
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
  "radius": 3.2,
  "$type": "Circle"
}
`))
		// 無理では？
		if err := dec.Decode(&s); err != nil {
			return err
		}
	}
	return nil
}
