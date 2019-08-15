package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"sync"
)

// TransparentErr :
type TransparentErr struct {
	Summary  string
	Messages []string
}

// Error :
func (e *TransparentErr) Error() string {
	return e.Summary
}

// MarshalJSON :
func (e *TransparentErr) MarshalJSON() ([]byte, error) {
	// TODO: verbose
	return []byte(fmt.Sprintf(`%q`, e.Summary)), nil
}

var (
	// ErrRequired :
	ErrRequired = &TransparentErr{Summary: "required"}
)

// ErrMap :
type ErrMap struct {
	mu  sync.Mutex
	Map map[string]error

	Fields []string
}

// Add :
func (e *ErrMap) Add(name string, err error) *ErrMap {
	defer e.mu.Unlock()
	e.mu.Lock()
	e.Fields = append(e.Fields, name)
	e.Map[name] = err
	return e
}

// Error :
func (e *ErrMap) Error() string {
	var b strings.Builder
	for _, name := range e.Fields {
		fmt.Fprintln(&b, name, e.Map[name].Error())
	}
	return b.String()
}

// MarshalJSON :
func (e *ErrMap) MarshalJSON() ([]byte, error) {
	var b bytes.Buffer
	encoder := json.NewEncoder(&b)
	b.WriteString("{")
	for i, name := range e.Fields {
		b.WriteString(`"` + name + `": `)
		if err := encoder.Encode(e.Map[name]); err != nil {
			return nil, err // xxx
		}
		if i < len(e.Fields)-1 {
			b.WriteString(",")
		}
	}
	b.WriteString("}")
	return b.Bytes(), nil
}

// New :
func New() *ErrMap {
	return &ErrMap{
		Map: map[string]error{},
	}
}

func main() {
	{
		err := New()
		err = err.Add("name", ErrRequired)
		err = err.Add("age", ErrRequired)
		fmt.Println(err.Error())
	}
	fmt.Println("----------------------------------------")
	{
		err := New()
		err = err.Add("name", ErrRequired)
		err = err.Add("age", ErrRequired)
		encoder := json.NewEncoder(os.Stdout)
		encoder.SetIndent("", "  ")
		fmt.Println(encoder.Encode(err))
	}
	fmt.Println("----------------------------------------")
	{
		root := New()
		err := root
		err = err.Add("name", ErrRequired)
		{
			err := New()
			err.Add("name", ErrRequired)
			root.Add("father", err)
		}
		{
			err := New()
			err.Add("name", ErrRequired)
			root.Add("mother", err)
		}
		encoder := json.NewEncoder(os.Stdout)
		encoder.SetIndent("", "  ")
		fmt.Println(encoder.Encode(err))
	}
}
