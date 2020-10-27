package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

type Cmd struct {
	Name    string
	SubCmds []string
}
type State struct {
	W  io.Writer
	lv int
}

func (s *State) Indent(fn func()) {
	s.lv++
	fn()
	s.lv--
}

type Gen struct {
	*State
	Q [][]string
}

func (g *Gen) p(pat string, args ...interface{}) {
	if pat == "" {
		io.WriteString(g.W, "\n")
		return
	}
	io.WriteString(g.W, strings.Repeat("  ", g.lv))
	fmt.Fprintf(g.W, pat, args...)
	io.WriteString(g.W, "\n")
}

func (g *Gen) Gen(d Data) {
	g.p(`function %[1]s() {`, d.Name)
	g.Indent(func() {
		g.p(`echo $@`)
	})
	g.p(`}`)
	g.p("")

	g.p(`function _%[1]s_completion() {`, d.Name)
	g.Indent(func() {
		g.Visit(d.Cmds[d.Name])
	})
	g.p(`}`)
	g.p("")
	g.p(`complete -F _%[1]scompletion %[1]s`, d.Name)
}

func (g *Gen) Visit(cmd Cmd) {
	g.p(`local cur prev`)
	g.p("")
	g.p(`cur=${COMP_WORDS[COMP_CWORD]}`)
	g.p(`prev=${COMP_WORDS[COMP_WORD-1]}`)
	g.p("")
	g.p(`case ${COMP_CWORD} in`)
	g.Indent(func() {
		g.visit(cmd, []string{cmd.Name}) // variables: cur, prev
		fmt.Println(g.Q)
	})
	g.p(`esac`)
}
func (g *Gen) visit(cmd Cmd, path []string) {
	g.p(`COMPREPLY=( $(compgen -W "%s" -- ${COMP_WORDS[COMP_CWORD]}) )`, strings.Join(cmd.SubCmds, " "))
	g.p(";;")
	for _, subcmd := range cmd.SubCmds {
		g.Q = append(g.Q, append(path, subcmd))
	}
}

type Data struct {
	Name string
	Cmds map[string]Cmd
}

func main() {
	g := &Gen{
		State: &State{W: os.Stdout},
	}
	g.Gen(Data{
		Name: "foo",
		Cmds: map[string]Cmd{
			"foo": Cmd{
				Name:    "foo",
				SubCmds: []string{"bar", "boo", "xxx"},
			},
		},
	})
}
