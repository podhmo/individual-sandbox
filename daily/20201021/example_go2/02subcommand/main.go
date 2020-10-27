package main

import (
	"log"
	"os"
	"strings"
	"text/template"
)

func main() {
	tmpl := template.Must(template.New("R").Funcs(map[string]interface{}{
		"join": strings.Join,
	}).Parse(`
function {{ .Name }}() {
    echo $@
}
function _{{ .Name }}_completion(){
    local cur prev

    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[COMP_CWORD-1]}

    case ${COMP_CWORD} in
        1)
{{ with $s := (index .States .Path) }}
            COMPREPLY=( $(compgen -W '{{ join $s.Candidates " " }}' -- ${COMP_WORDS[COMP_CWORD]}) )
            ;;
{{- end }}
        2)
            case ${prev} in
                bar )
{{ with $s := (index .States "bar") }}
                    COMPREPLY=( $(compgen -W '{{ join $s.Candidates " " }}' -- ${COMP_WORDS[COMP_CWORD]}) )
{{- end }}
                    ;;
                boo )
{{ with $s := (index .States "boo") }}
                    COMPREPLY=( $(compgen -W '{{ join $s.Candidates " " }}' -- ${COMP_WORDS[COMP_CWORD]}) )
{{- end }}
                    ;;
                bxxx )
{{ with $s := (index .States "bxxx") }}
                    COMPREPLY=( $(compgen -W '{{ join $s.Candidates " " }}' -- ${COMP_WORDS[COMP_CWORD]}) )
{{- end }}
                    ;;
                xxx )
{{ with $s := (index .States "xxx") }}
                    COMPREPLY=( $(compgen -W '{{ join $s.Candidates " " }}' -- ${COMP_WORDS[COMP_CWORD]}) )
{{- end }}
                    ;;
            esac
    esac
}
complete -F _{{ .Name }}_completion {{ .Name }}
`))

	type State struct {
		Name       string
		Candidates []string
	}

	states := []State{
		{
			Name:       "",
			Candidates: []string{"bar", "boo", "bxxx xxx"},
		},
		{
			Name:       "bar",
			Candidates: []string{"one", "two", "three", "bar"},
		},
		{
			Name:       "bar",
			Candidates: []string{"one", "two", "three", "bar"},
		},
		{
			Name:       "boo",
			Candidates: []string{"one", "two", "three", "boo"},
		},
		{
			Name:       "bxxx",
			Candidates: []string{"one", "two", "three", "bxxx"},
		},
		{
			Name:       "xxx",
			Candidates: []string{"one", "two", "three", "xxx"},
		},
	}
	params := struct {
		Name   string
		Path   string
		States map[string]State
	}{
		Name:   "foo",
		Path:   "",
		States: map[string]State{},
	}
	for _, s := range states {
		params.States[s.Name] = s
	}

	if err := tmpl.Execute(os.Stdout, params); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
