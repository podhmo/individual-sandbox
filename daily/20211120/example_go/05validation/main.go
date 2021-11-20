package main

import (
	"fmt"
	"strings"
)

type Person struct {
	Name string `json:"name"`
}

func main() {
	p := Person{}
	fmt.Println(ValidatePerson(&p, "Prefix."))
}

func ValidatePerson(ob *Person, prefix string) *ErrorSet {
	perrs := new(ErrorSet)
	t := &Translator{Lang: "ja"}
	if path, value := prefix+"Name", ob.Name; value == zstring {
		*perrs = append(*perrs, &FieldError{Translator: t, Key: "name", Path: path, Tag: "required", Value: value})
	}
	return perrs.NilOrError()
}

var zstring string

type ErrorSet []error

func (es ErrorSet) NilOrError() *ErrorSet {
	if len(es) > 0 {
		return &es
	}
	return nil
}

func (es ErrorSet) Error() string {
	r := make([]string, len(es))
	for i, e := range es {
		r[i] = e.Error()
	}
	return strings.Join(r, "\n")
}

type Translator struct {
	Lang string
}

func (t *Translator) Translate(fe *FieldError) string {
	return map[string]string{
		"ja": "空文字列は許されません",
		"":   "not blank",
	}[t.Lang]
}

type FieldError struct {
	*Translator
	Key   string
	Path  string
	Tag   string
	Value interface{}
}

func (e *FieldError) Error() string {
	// return fmt.Sprintf("%s: %s:%s", e.Path, e.Name, e.Translate(e))
	return fmt.Sprintf(FieldErrMsg, e.Key, e.Translate(e), e.Tag)
}

var (
	FieldErrMsg = "Key: '%s' Error:Field validation for '%s' failed on the '%s' tag"
)
