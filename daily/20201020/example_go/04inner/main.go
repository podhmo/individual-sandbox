package main

import (
	"bytes"
	"os"
	"text/template"
)

func main() {
	type S struct {
		Ans int
		N   int
	}
	var tmpl *template.Template
	tmpl = template.Must(template.New("root").
		Funcs(map[string]interface{}{
			"dict": func(args ...interface{}) map[interface{}]interface{} {
				d := map[interface{}]interface{}{}
				for i := 0; i < len(args); i += 2 {
					d[args[i]] = args[i+1]
				}
				return d
			},
			"add": func(x, y int) int { return x + y },
			"use": func(name string, d map[interface{}]interface{}) (string, error) {
				var b bytes.Buffer
				err := tmpl.ExecuteTemplate(&b, name, d)
				return b.String(), err
			},
		}).
		Parse(`
{{- define "Box" -}}
<Box>{{ . }}</Box>
{{- end }}
{{- define "NBox" -}}
{{ if (ge 0 .N) }}{{ .Content }}{{ else }}<Box n="{{.N}}">{{ template "NBox" (dict "N" (add .N -1) "Content" .Content)}}</Box>{{ end }}
{{- end }}
{{template "Box" .Message}}
{{template "NBox" (dict "N" 3 "Content" .Message)}}
{{template "NBox" (dict "N" 2 "Content" (use "NBox" (dict "N" 3 "Content" .Message)))}}
{{- with $inner := (use "NBox" (dict "N" 3 "Content" .Message)) }}
{{template "NBox" (dict "N" 2 "Content" $inner)}}
{{end}}
`))
	tmpl.Execute(os.Stdout, map[string]string{"Message": "hello"})
}
