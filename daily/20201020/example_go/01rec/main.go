package main

import (
	"os"
	"text/template"
)

func main() {
	tmpl := template.Must(template.New("root").
		Funcs(map[string]interface{}{
			"add": func(x, y int) int { return x + y },
		}).
		Parse(`
{{- define "f" N -}}
{{ if (ge 1 .) }} 1 {{ else }} {{.}} * {{ template "f" (add . -1) }} {{ end }}
{{- end -}}
{{- template "f" .N }}
`))
	tmpl.Execute(os.Stdout, map[string]int{"N": 10})
}
