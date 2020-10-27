package main

import (
	"log"
	"os"
	"strconv"
	"strings"
	"text/template"
)

func main() {
	tmpl := template.Must(template.New("R").Funcs(map[string]interface{}{
		"join": func(args []string, sep string) string {
			quoted := make([]string, len(args))
			for i := range args {
				quoted[i] = strconv.Quote(args[i])
			}
			return strings.Join(quoted, sep)
		},
	}).Parse(`
function {{ .Name }}() {
    echo $@
}
function _{{ .Name }}_completion(){
      COMPREPLY=( $(compgen -W '{{ join .Args " " }}' -- ${COMP_WORDS[COMP_CWORD]}) );
}
complete -F _{{ .Name }}_completion {{ .Name }}
`))

	type Params struct {
		Name string
		Args []string
	}

	params := Params{
		Name: "foo",
		Args: []string{"bar", "boo", "bxxx xxx"},
	}
	if err := tmpl.Execute(os.Stdout, params); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
