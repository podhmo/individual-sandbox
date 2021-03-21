package main

import (
	"encoding/json"
	"fmt"
	"reflect"
)

type Person struct {
	Name string `json:"name" validate:"maxLength=255,pattern=[A-Z]\\S+"`
}

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

type Registry struct {
	Registry map[reflect.Type]string
}

// Name :
type Name string

func (r *Registry) RegisterType(rt reflect.Type, s string) {
	r.Registry[rt] = s
}

var defaultRegistry *Registry

func init() {
	defaultRegistry = &Registry{
		Registry: map[reflect.Type]string{},
	}
	defaultRegistry.RegisterType(reflect.TypeOf(Name("")), `maxLength=255`)
}

func main() {
}
