package main

import (
	"fmt"
	"strings"
	"sync"

	"github.com/go-playground/validator/v10"
)

type Color string

const (
	ColorRed   Color = "red"
	ColorGreen       = "green"
	ColorBlue        = "blue"
)

func ValidateColor(fl validator.FieldLevel) bool {
	s := fl.Field().String()
	for _, c := range []string{"red", "green", "blue"} {
		if s == c {
			return true
		}
	}
	return false
}

var (
	mu        sync.RWMutex
	enumCache = map[string][]string{}
)

func ValidateEnum(fl validator.FieldLevel) bool {
	tagValue := fl.Param()
	mu.RLock()
	enums, ok := enumCache[tagValue]
	mu.RUnlock()
	if !ok {
		mu.Lock()
		enums = strings.Split(tagValue, ":")
		enumCache[tagValue] = enums
		mu.Unlock()
	}

	s := fl.Field().String()
	for _, c := range enums {
		if s == c {
			return true
		}
	}
	return false
}

type S struct {
	Color  string `validate:"is-color"`
	Color2 string `validate:"enum=red:green:blue"`
	Color3 string `validate:"oneof=red green blue"`
}

func main() {
	validate := validator.New()
	validate.RegisterValidation("is-color", ValidateColor)
	validate.RegisterValidation("enum", ValidateEnum)

	{
		s := S{Color: "red", Color2: "red", Color3: "red"}
		fmt.Println("red", validate.Struct(s))
	}
	{
		s := S{Color: "black", Color2: "black", Color3: "black"}
		fmt.Println("black", validate.Struct(s))
	}
}
