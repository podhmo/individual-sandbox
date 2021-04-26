package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"os"
)

type ShapeRef struct {
	Shape
}

func (s *ShapeRef) MarshalJSON() ([]byte, error) {
	b, err := json.Marshal(s.Shape)
	if err != nil {
		return nil, err
	}

	buf := bytes.NewBuffer(b[:len(b)-1]) // trim "}"
	if _, err := fmt.Fprintf(buf, `, "$type": %q}`, s.Type()); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func (s *ShapeRef) UnmarshalJSON(b []byte) error {
	var w struct {
		Type ShapeType `json:"$type"`
	}
	if err := json.Unmarshal(b, &w); err != nil {
		return err
	}

	switch w.Type {
	case ShapeTypeRectangle:
		s.Shape = &Rectangle{}
	case ShapeTypeCircle:
		s.Shape = &Circle{}
	default:
		return fmt.Errorf("unexpected shape %T", w.Type)
	}
	return json.Unmarshal(b, s.Shape)
}

type Shape interface {
	Type() ShapeType
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

func (*Rectangle) Type() ShapeType {
	return ShapeTypeRectangle
}

type Circle struct {
	Radius float64 `json:"radius"`
}

func (*Circle) Type() ShapeType {
	return ShapeTypeCircle
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	{
		s := &ShapeRef{&Circle{Radius: 3.2}}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(s); err != nil {
			return err
		}
	}

	{
		var ref ShapeRef
		dec := json.NewDecoder(bytes.NewBufferString(`
	{
	  "radius": 3.2,
	  "$type": "Circle"
	}
	`))
		if err := dec.Decode(&ref); err != nil {
			return err
		}
		fmt.Printf("%#+v\n", ref)
		fmt.Printf("%#+v\n", ref.Shape.(*Circle))
	}
	return nil
}
