package main

import (
	"os"
	"text/template"
)

func main() {
	type S struct {
		Ans int
		N   int
	}
	tmpl := template.Must(template.New("root").
		Funcs(map[string]interface{}{
			"add": func(x, y int) int { return x + y },
			"mul": func(x, y int) int { return x * y },
			"S":   func(ans, n int) S { return S{Ans: ans, N: n} },
		}).
		Parse(`
{{- define "f" -}}
{{ if (ge 1 .N) }} 1 = {{.Ans}} {{ else }} {{.N}} * {{ template "f" (S (mul .Ans .N) (add .N -1)) }} {{ end }}
{{- end -}}
{{- template "f" (S 1 .N)}}
`))
	tmpl.Execute(os.Stdout, map[string]int{"N": 10})
}
