package main

import (
	"encoding/json"
	"fmt"
)

type FullError struct {
	Code    int     `json:"code"`
	Message string  `json:"message"`
	Errors  []error `json:"errors"`
}

func (e *FullError) Error() string {
	b, err := json.Marshal(e)
	if err != nil {
		return fmt.Sprintf(`{"code": 500, "message": %q}`, err)
	}
	return string(b)
}

type Error struct {
	Path    []string `json:"path"`
	Message string   `json:"message"`
}

func (e *Error) Error() string {
	return fmt.Sprintf(`{"path": %q, "message": %q}`, e.Path, e.Message)
}

func main() {
	err := &FullError{
		Code:    400,
		Message: "bad input",
		Errors: []error{
			&Error{Path: []string{"children", "0", "name"}, Message: "required"},
		},
	}
	fmt.Println(err.Error())
}
