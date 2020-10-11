package main

import (
	"encoding/json"
	"os"

	"github.com/go-playground/validator/v10"
)

// User contains user information
type User struct {
	FirstName string `validate:"required"`
	LastName  string `validate:"required"`
	Age       uint8  `validate:"gte=0,lte=130"`

	Father *User
}

// use a single instance of Validate, it caches struct info
var validate *validator.Validate

func main() {
	validate = validator.New()

	validateStruct()
}

type APIError struct {
	Status  int      `json:"status"`
	Message string   `json:"message"`
	Details []string `json:"details"`
}

func ConvertToAPIError(err error) APIError {
	// TODO: if DEBUG=1, with stack trace?
	// TODO: customize status value
	// TODO: customize message value
	errs, ok := err.(validator.ValidationErrors)
	if !ok {
		return APIError{
			Status:  500,
			Message: "internal error",
			Details: []string{},
		}
	}

	details := make([]string, len(errs))
	for i := range errs {
		details[i] = errs[i].Error()
	}
	return APIError{
		Status:  400,
		Message: details[0],
		Details: details,
	}
}
func validateStruct() {
	user := &User{
		FirstName: "",
		LastName:  "Smith",
		Age:       135,
		Father:    &User{},
	}

	err := validate.Struct(user)
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	encoder.Encode(ConvertToAPIError(err))
}
