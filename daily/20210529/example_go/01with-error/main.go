package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strings"
	"time"
)

// 型付きID
// あんまり速度のことは考えていない状態

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	var item Todo
	s := `{"title": "go to bed", "id": "t_1234", "parent_id": "t_1"}`

	fmt.Println(">>>", s)
	if err := json.Unmarshal([]byte(s), &item); err != nil {
		return err
	}
	fmt.Printf("--- %#+v\n", item)

	fmt.Print("<<< ")
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(item)
}

type ParseError struct {
	Message string
	Input   string
	Output  interface{}
}

func NewInvalidInput(ob interface{}, v string) *ParseError {
	return &ParseError{Output: ob, Input: v, Message: "invalid input"}
}
func (err *ParseError) Error() string {
	return fmt.Sprintf("parse error, %s -- %q for %T", err.Message, err.Input, err.Output)
}

type TodoID string

func (id *TodoID) UnmarshalText(b []byte) error {
	s := string(b)
	if s == "null" {
		return nil
	}

	// ここでvalidationを実行すると嬉しくないかもしれない
	if len(b) <= 2 || !strings.HasPrefix(s, "t_") {
		return NewInvalidInput(id, s)
	}
	*id = TodoID(s[2:])
	return nil
}

func (id TodoID) MarshalText() ([]byte, error) {
	return []byte("t_" + id), nil
}

type Todo struct {
	ID    TodoID `json:"id"`
	Title string `json:"title"`

	ParentID   *TodoID    `json:"parent_id"`
	FinishedAt *time.Time `json:"finished_at"`
}
