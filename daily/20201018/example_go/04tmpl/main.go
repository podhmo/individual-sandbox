package main

import (
	"log"
	"os"
	"text/template"
)

type Thing struct {
	Text   string
	Things []Thing
}

func main() {
	w := os.Stdout
	tmpl, err := template.New("root").Parse(`<html>
  {{ define "message" -}}
	<li>{{ .Text -}}
	  {{ if gt (len .Things) 0 -}}
		<ul>
		  {{ range .Things -}}
		  {{ template "message" . -}}
		  {{ end -}}
		</ul>
	  {{ end -}}
	</li>
  {{ end -}}

  <ul>{{ template "message" . }}</ul>
</html>
	`)
	data := Thing{
		Text: "Hello",
		Things: []Thing{
			{
				Text: "World",
			},
			{
				Text: "World?",
			},
		},
	}
	err = tmpl.Execute(w, data)
	if err != nil {
		log.Println("Error:", err)
	}
}
