package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"strings"

	"github.com/go-playground/locales/en"
	ut "github.com/go-playground/universal-translator"
	"github.com/go-playground/validator/v10"
	en_translations "github.com/go-playground/validator/v10/translations/en"
)

type Person struct {
	Name   string  `json:"name" validate:"required"`
	Age    int     `json:"age" validate:"required"`
	Father *Person `json:"father"`

	// todo: dive
}

// use a single instance , it caches struct info
var (
	uni      *ut.UniversalTranslator
	validate *validator.Validate
)

// validationErrorsTranslations

func main() {
	en := en.New()
	uni = ut.New(en, en)

	// this is usually know or extracted from http 'Accept-Language' header
	// also see uni.FindTranslator(...)
	trans, _ := uni.GetTranslator("en")

	validate = validator.New()
	validate.RegisterTagNameFunc(func(fld reflect.StructField) string {
		name := strings.SplitN(fld.Tag.Get("json"), ",", 2)[0]
		if name == "-" {
			return ""
		}
		return name
	})

	en_translations.RegisterDefaultTranslations(validate, trans)

	{
		var p Person
		p.Father = &Person{}

		ctx := context.Background()
		err := validate.StructCtx(ctx, p)

		fmt.Println("----------------------------------------")
		fmt.Println(err)
		fmt.Println("----------------------------------------")
		transMap := err.(validator.ValidationErrors).Translate(trans)
		for k, v := range transMap {
			fmt.Printf("%q: %q\n", k, v)
		}

		fmt.Println("----------------------------------------")
		for _, fe := range err.(validator.ValidationErrors) {
			fmt.Println(fe.Field(), fe.Translate(trans))
		}

		fmt.Println("----------------------------------------")
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		r := make([]interface{}, len(err.(validator.ValidationErrors)))
		for i, fe := range err.(validator.ValidationErrors) {
			r[i] = map[string]string{"field": fe.Field(), "path": fe.StructNamespace(), "message": fe.Translate(trans)}
		}
		if err := enc.Encode(r); err != nil {
			fmt.Println("!!", err)
		}		
	}
}
