package runtime

import (
	"fmt"
	"strings"
)

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

func (t *Translator) Translate(tag string, value interface{}) string {
	switch tag {
	case "required":
		return map[string]string{
			"ja": "空文字列は許されません",
			"":   "not blank",
		}[t.Lang]
	case "":
		return fmt.Sprintf("%s", value)
	default:
		return fmt.Sprintf("unexpected tag: %s", tag) // xxx
	}
}

type FieldError struct {
	*Translator
	Key   string
	Path  string
	Tag   string
	Value interface{}
}

func (e *FieldError) Error() string {
	return fmt.Sprintf(FieldErrMsg, e.Path, e.Tag, e.Translate(e.Tag, e.Value))
}
func (e *FieldError) SetTranslator(t *Translator) {
	e.Translator = t
}

var (
	FieldErrMsg = "Path: '%s' Error:Field validation for '%s' Message:'%s'"
)
