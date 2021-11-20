package main

import (
	"github.com/go-playground/locales/ja"
	ut "github.com/go-playground/universal-translator"
	"github.com/go-playground/validator/v10"
	ja_translations "github.com/go-playground/validator/v10/translations/ja"
)

func main() {

	ja := ja.New()
	uni := ut.New(ja, ja)
	trans, _ := uni.GetTranslator("ja")

	validate := validator.New()
	ja_translations.RegisterDefaultTranslations(validate, trans)

	trans
}
