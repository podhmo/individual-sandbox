package main

import (
	"fmt"

	"github.com/go-playground/validator/v10"
)

// User contains user information
type User struct {
	FirstName string `validate:"required"`
	LastName  string `validate:"required"`
	Age       uint8  `validate:"gte=0,lte=130"`
}

// use a single instance of Validate, it caches struct info
var validate *validator.Validate

func main() {
	validate = validator.New()

	validateStruct()
}

func validateStruct() {
	user := &User{
		FirstName: "",
		LastName:  "Smith",
		Age:       135,
	}

	err := validate.Struct(user)
	fmt.Printf("err: %#+v\n", err)
	// validator.ValidationErrors{(*validator.fieldError)(0xc000144480), (*validator.fieldError)(0xc000144510)}
	fmt.Println(err)

	// Key: 'User.FirstName' Error:Field validation for 'FirstName' failed on the 'required' tag
	// Key: 'User.Age' Error:Field validation for 'Age' failed on the 'lte' tag
}

// // doc

// package validator // import "github.com/go-playground/validator/v10"

// type ValidationErrors []FieldError
//     ValidationErrors is an array of FieldError's for use in custom error
//     messages post validation.

// func (ve ValidationErrors) Error() string
//     Error is intended for use in development + debugging and not intended to be
//     a production error message. It allows ValidationErrors to subscribe to the
//     Error interface. All information to create an error message specific to your
//     application is contained within the FieldError found within the
//     ValidationErrors array

// func (ve ValidationErrors) Translate(ut ut.Translator) ValidationErrorsTranslations
//     Translate translates all of the ValidationErrors

// ----------------------------------------

// package validator // import "github.com/go-playground/validator/v10"

// type FieldError interface {

// 	// returns the validation tag that failed. if the
// 	// validation was an alias, this will return the
// 	// alias name and not the underlying tag that failed.
// 	//
// 	// eg. alias "iscolor": "hexcolor|rgb|rgba|hsl|hsla"
// 	// will return "iscolor"
// 	Tag() string

// 	// returns the validation tag that failed, even if an
// 	// alias the actual tag within the alias will be returned.
// 	// If an 'or' validation fails the entire or will be returned.
// 	//
// 	// eg. alias "iscolor": "hexcolor|rgb|rgba|hsl|hsla"
// 	// will return "hexcolor|rgb|rgba|hsl|hsla"
// 	ActualTag() string

// 	// returns the namespace for the field error, with the tag
// 	// name taking precedence over the field's actual name.
// 	//
// 	// eg. JSON name "User.fname"
// 	//
// 	// See StructNamespace() for a version that returns actual names.
// 	//
// 	// NOTE: this field can be blank when validating a single primitive field
// 	// using validate.Field(...) as there is no way to extract it's name
// 	Namespace() string

// 	// returns the namespace for the field error, with the field's
// 	// actual name.
// 	//
// 	// eq. "User.FirstName" see Namespace for comparison
// 	//
// 	// NOTE: this field can be blank when validating a single primitive field
// 	// using validate.Field(...) as there is no way to extract its name
// 	StructNamespace() string

// 	// returns the fields name with the tag name taking precedence over the
// 	// field's actual name.
// 	//
// 	// eq. JSON name "fname"
// 	// see StructField for comparison
// 	Field() string

// 	// returns the field's actual name from the struct, when able to determine.
// 	//
// 	// eq.  "FirstName"
// 	// see Field for comparison
// 	StructField() string

// 	// returns the actual field's value in case needed for creating the error
// 	// message
// 	Value() interface{}

// 	// returns the param value, in string form for comparison; this will also
// 	// help with generating an error message
// 	Param() string

// 	// Kind returns the Field's reflect Kind
// 	//
// 	// eg. time.Time's kind is a struct
// 	Kind() reflect.Kind

// 	// Type returns the Field's reflect Type
// 	//
// 	// // eg. time.Time's type is time.Time
// 	Type() reflect.Type

// 	// returns the FieldError's translated error
// 	// from the provided 'ut.Translator' and registered 'TranslationFunc'
// 	//
// 	// NOTE: if no registered translator can be found it returns the same as
// 	// calling fe.Error()
// 	Translate(ut ut.Translator) string

// 	// Error returns the FieldError's message
// 	Error() string
// }
//     FieldError contains all functions to get error details

