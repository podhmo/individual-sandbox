package clientgen

import (
	"fmt"
	"go/parser"
	"go/printer"
	"go/token"
	"io"
	"io/ioutil"
	"m/glue"
	"os"
	"path/filepath"
	"strconv"
)

type Config struct {
	GetOutput func(name string) io.Writer
	Name      string
}

func (c *Config) Gen(g *glue.Glue) error {
	if c.GetOutput == nil {
		c.GetOutput = func(name string) io.Writer {
			fmt.Fprintln(os.Stdout, "//", name)
			return os.Stdout
		}
	}
	if c.Name == "" {
		c.Name = "gen"
	}

	fset := token.NewFileSet()
	dir := filepath.Dir(FileName(NewDecoder))

	{
		filename := filepath.Join(dir, "decoder.go")
		source, err := ioutil.ReadFile(filename)
		if err != nil {
			return fmt.Errorf("read file %w", err)
		}
		f, err := parser.ParseFile(fset, filename, source, parser.ParseComments)
		if err != nil {
			return fmt.Errorf("parse file %w", err)
		}

		f.Name.Name = c.Name

		w := c.GetOutput("decoder.go")
		p := &printer.Config{Tabwidth: 8}
		if err := p.Fprint(w, fset, f); err != nil {
			return fmt.Errorf("print file %w", err)
		}
	}

	{
		fname := filepath.Join(dir, "actions.tmpl.go")
		f, err := parser.ParseFile(fset, fname, nil, parser.ParseComments)
		if err != nil {
			return fmt.Errorf("parse file %w", err)
		}

		f.Name.Name = c.Name
		// ast.Fprint(os.Stdout, fset, f, nil)

		tmpl, err := BuildTemplate(fset, fname, f)
		if err != nil {
			return fmt.Errorf("build template %w", err)
		}

		w := c.GetOutput("list_todo_action.go")
		data := Data{
			Method:     strconv.Quote("GET"),
			Path:       strconv.Quote("/todo"),
			Action:     "ListTodo",
			ReturnType: "[]Todo",
		}
		if err := tmpl.Execute(w, data); err != nil {
			return fmt.Errorf("execute template %w", err)
		}
	}
	return nil
}

type Data struct {
	Method string
	Path   string

	Action     string
	ReturnType string
}
