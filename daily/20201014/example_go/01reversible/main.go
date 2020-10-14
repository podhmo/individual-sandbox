package main

import (
	"fmt"
	"io"
	"os"
	"time"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

type Person struct {
	Name      string    `json:"name"`
	Age       int       `json:"age"`
	CreatedAt time.Time `json:"createdAt"`
}

type Emitter struct {
	Writer io.Writer // TODO: separated output
}

func (e *Emitter) Emit(s shape.Shape) error {
	// todo: import

	switch s := s.(type) {
	case shape.Struct:
		w := e.Writer
		nl := []byte{'\n'}
		fmt.Fprintf(w, "type %s struct {\n", s.GetName())
		for i, name := range s.Fields.Keys {
			f := s.Fields.Values[i]
			fmt.Fprintf(w, "	%s %s", name, f.GetReflectType())
			if string(s.Tags[i]) != "" {
				fmt.Fprintf(w, " `%s`", s.Tags[i])
			}
			w.Write(nl)
		}
		fmt.Fprintf(w, "}\n")
	default:
		return fmt.Errorf("unsupported type %T", s)
	}
	return nil // never
}

func main() {
	s := shape.Extract(Person{})
	(&Emitter{Writer: os.Stdout}).Emit(s)
}
