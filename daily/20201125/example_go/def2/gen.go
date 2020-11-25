package def2

import (
	"io"
	"m/astmpl"
)

// Variables: Name, NameParams, Template

type __Name__Params astmpl.REPLACED

type __Name__Definition struct {
	Template string
	Execute  func(io.Writer, __Name__Params) error
	New      func() __Name__Params
}

func (l *Lookup) __Name__() __Name__Definition {
	var def __Name__Definition
	def = __Name__Definition{
		New: func() __Name__Params {
			var p __Name__Params
			return p
		},
		Template: `{{.Template}}`,
		Execute: func(w io.Writer, params __Name__Params) error {
			return l.ExecuteTemplate(w, params, "{{.Name}}", def.Template)
		},
	}
	return def
}
