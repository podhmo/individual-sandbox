package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

// Priority ...
type Priority int

const (
	PriorityHigh   Priority = 10
	PriorityNormal          = 0
	PriorityLow             = -10
)

// MapErr ...
type MapErr struct {
	Message     string              `json:"message"`
	Fields      map[string][]string `json:"fields,omitempty"`
	MaxPriority Priority            `json:"-"`
}

// Add ...
func (e *MapErr) Add(name string, priority Priority, message string) *MapErr {
	if e == nil {
		e = &MapErr{
			Message:     message,
			Fields:      map[string][]string{},
			MaxPriority: priority,
		}
	}
	e.Fields[name] = append(e.Fields[name], message)
	if e.MaxPriority < priority {
		e.MaxPriority = priority
		e.Message = message
	}
	return e
}

// Untyped ...
func (e *MapErr) Untyped() error {
	if e == nil {
		return nil
	}
	return e
}

// Error ...
func (e *MapErr) Error() string {
	var b strings.Builder
	if prettyPrint {
		e.writeMultiline(&b)
	} else {
		e.writeSingleline(&b)
	}
	return b.String()
}

// Format ...
func (e *MapErr) Format(f fmt.State, c rune) {
	fmt.Fprintf(f, "MapErr:")
	if c == 'v' && f.Flag('+') {
		e.writeMultiline(f)
	} else {
		e.writeSingleline(f)
	}
}

func (e *MapErr) writeSingleline(w io.Writer) {
	encoder := json.NewEncoder(w)
	if err := encoder.Encode(e); err != nil {
		fmt.Fprintf(w, `{"error": %q}`, err)
	}
}

func (e *MapErr) writeMultiline(w io.Writer) {
	encoder := json.NewEncoder(w)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(e); err != nil {
		fmt.Fprintf(w, `{"error": %q}`, err)
	}
}

var prettyPrint bool

func init() {
	prettyPrint, _ = strconv.ParseBool(os.Getenv("DEBUG"))
}

func run() error {
	var err *MapErr
	// return err.Untyped()
	return err.Add("xxx", PriorityNormal, "XXX")
}

func main() {
	err := run()

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")

	fmt.Println("========================================")
	encoder.Encode(err)
	fmt.Println("========================================")

	fmt.Println(err)
	fmt.Printf("%+v", err)
	if err != nil {
		log.Fatalf("%+v", err)
	}
}
