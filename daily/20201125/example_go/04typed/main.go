package main

import (
	"fmt"
	"html/template"
	"io"
	"log"
	"os"
	"sync"

	"github.com/go-playground/validator"
)

type Lookup struct {
	validate *validator.Validate
	registry map[string]*template.Template
	mu       sync.Mutex
}

func (l *Lookup) ExecuteTemplate(w io.Writer, params interface{}, name string, body string) error {
	l.mu.Lock()
	defer l.mu.Unlock()
	tmpl, ok := l.registry[name]
	if !ok {
		t, err := template.New(name).Parse(body)
		if err != nil {
			return fmt.Errorf("parse: %w", err)
		}
		tmpl = t
		l.registry[name] = tmpl
	}
	if err := l.validate.Struct(params); err != nil {
		return fmt.Errorf("validate: %w", err)
	}
	return tmpl.Execute(w, params)
}

type FooDefinition struct {
	Template string
	Execute  func(io.Writer, FooParams) error
	New      func() FooParams
}

func (l *Lookup) Foo() FooDefinition {
	var def FooDefinition
	def = FooDefinition{
		New: func() FooParams {
			var p FooParams
			return p
		},
		Template: `{{ range $i, $x := .Items -}}
- {{ $x }}
{{ end -}}
`,
		Execute: func(w io.Writer, params FooParams) error {
			return l.ExecuteTemplate(w, params, "Foo", def.Template)
		},
	}
	return def
}

type FooParams struct {
	Name  string   `validate:"required"`
	Items []string `validate:"required"`
}

func New() *Lookup {
	return &Lookup{
		validate: validator.New(),
		registry: map[string]*template.Template{},
	}
}

func main() {
	lookup := New()
	def := lookup.Foo()
	params := def.New()
	params.Name = "xxx"
	params.Items = []string{"a", "b", "c"}
	if err := def.Execute(os.Stdout, params); err != nil {
		log.Fatalf("!!%+v", err)
	}
}
