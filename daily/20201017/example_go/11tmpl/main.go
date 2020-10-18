package main

import (
	"html/template"
	"os"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

func main() {
	tmpl := template.Must(template.New("action").Parse(`
func (c *Client) {{.Name}}(options ...{{.Name}}InputOption) {{.ReturnType}} {
	input := &{{.Name}}Input{
		ListInput: NewListInput(),
	}
	for _, opt := range options {
		opt.Apply{{.Name}}Input(input)
	}

	url := c.BaseURL + "{{.Path}}"
	req, err := c.Driver.NewRequest("{{.Method}}", url, input.RequestInput(nil))
	if err != nil {
		return nil, err
	}

	resp, err, cleanup := c.Driver.Do(req)
	if err != nil {
		return nil, err
	}
	defer cleanup()

	if err := c.Decoder.DecodeError(resp); err != nil {
		return nil, err
	}

	var items {{.ReturnType}}
	if c.Decoder.DecodeResult(resp, &items); err != nil {
		return nil, err
	}
	return items, nil
}
`))

	// support func(context.Context, <T>) ([]<T>, error)
	s := shape.Extract(ListFoo)
	tmpl.Execute(os.Stdout, map[string]string{
		"Method":     "GET",
		"Path":       "/todo",
		"Name":       s.GetName(),
		"ReturnType": s.(shape.Function).Returns.Values[0].GetReflectType().String(),
	})
}

type Foo struct {
}

func ListFoo() []Foo { return nil }
