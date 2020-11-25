package def2

import (
	"fmt"
	"html/template"
	"io"
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

func New() *Lookup {
	return &Lookup{
		validate: validator.New(),
		registry: map[string]*template.Template{},
	}
}
