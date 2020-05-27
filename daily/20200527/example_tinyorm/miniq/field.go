package miniq

import (
	"fmt"
	"strings"
)

type Field interface {
	Name() string
}
type As struct {
	NewName string
	Field   Field
}

func (as As) Name() string {
	return fmt.Sprintf("%s as %s", as.Field.Name(), as.NewName)
}

type Int64Field string

func (f Int64Field) Name() string {
	return string(f)
}
func (f Int64Field) As(name string) As {
	return As{NewName: name, Field: f}
}
func (f Int64Field) Compare(op string, value int64) *Bop {
	return &Bop{
		Op:           op, // e.g. "= ?"
		Left:         f.Name(),
		Right:        value,
		WithoutParen: true,
	}
}

type StringField string

func (f StringField) Name() string {
	return string(f)
}
func (f StringField) As(name string) As {
	return As{NewName: name, Field: f}
}
func (f StringField) Compare(op string, value string) *Bop {
	return &Bop{
		Op:           op, // e.g. "= ?"
		Left:         f.Name(),
		Right:        value,
		WithoutParen: true,
	}
}

type Call struct {
	Prefix string
	Args   []Field
}

func (c Call) Name() string {
	names := make([]string, len(c.Args))
	for i, f := range c.Args {
		names[i] = f.Name()
	}
	return fmt.Sprintf("%s(%s)", c.Prefix, strings.Join(names, ", "))
}

// concreate
func Count(values ...Field) Call {
	return Call{Prefix: "COUNT", Args: values}
}

type Literal string

func (v Literal) Name() string {
	return string(v)
}

func Literalf(fmt string, args ...interface{}) LiteralFormat {
	return LiteralFormat{
		Format: fmt,
		Args:   args,
	}
}

type LiteralFormat struct {
	Format string
	Args   []interface{}
}

func (v LiteralFormat) Name() string {
	return fmt.Sprintf(v.Format, v.Args...)
}
func (v LiteralFormat) As(name string) As {
	return As{Field: v, NewName: name}
}

var STAR = StringField("*")
