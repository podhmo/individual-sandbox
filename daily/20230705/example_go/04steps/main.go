package main

import (
	"bytes"
	"fmt"
	"strings"
)

type T struct {
	buf   bytes.Buffer
	lv    int
	stack [][]string
	vars  map[string]string
}

func (t *T) Msg(msg string) {
	id := fmt.Sprintf("v%d", len(t.vars))
	t.vars[id] = msg
	t.stack[len(t.stack)-1] = append(t.stack[len(t.stack)-1], id)
}

func (t *T) Run(name string, fn func(t *T)) {
	w := &t.buf
	t.stack[len(t.stack)-1] = append(t.stack[len(t.stack)-1], name)

	fmt.Fprintf(w, "%ssubgraph %s\n", strings.Repeat("  ", t.lv), name)
	defer func() {
		fmt.Fprintf(w, "%send\n", strings.Repeat("  ", t.lv))
	}()

	t.lv++
	if fn != nil {
		t.stack = append(t.stack, []string{}) // push
		fn(t)
		last := t.stack[len(t.stack)-1]
		for _, id := range last {
			msg, ok := t.vars[id]
			if ok {
				fmt.Fprintf(w, "%s%s[%q];\n", strings.Repeat("  ", t.lv), id, EscapeString(msg))
			}
		}
		if len(last) > 1 {
			fmt.Fprintf(w, "%s%s;\n", strings.Repeat("  ", t.lv), strings.Join(last, " --> "))
		}
		t.stack = t.stack[:len(t.stack)-1]
	}
	t.lv--
}

func Run(fn func(t *T)) {
	t := &T{stack: [][]string{{}}, vars: map[string]string{}}
	w := &t.buf

	fmt.Fprintln(w, "```mermaid")
	defer func() {
		fmt.Fprintln(w, "```")
		fmt.Println(t.buf.String())
	}()

	fmt.Fprintln(w, "flowchart TD") // TODO: support LR
	t.lv++
	if fn != nil {
		fn(t)
		last := t.stack[len(t.stack)-1]
		if len(last) > 1 {
			fmt.Fprintf(w, "%s%s;\n", strings.Repeat("  ", t.lv), strings.Join(last, " --> "))
		}
	}
	t.lv--
}

var textEscaper = strings.NewReplacer(
	`&`, "＆",
	`'`, "’",
	`<`, "＜;",
	`>`, "＞;",
	`"`, "”",
)

func EscapeString(s string) string {
	return textEscaper.Replace(s)
}

func main() {
	Run(func(t *T) {
		t.Run("hello", func(t *T) {
			t.Msg("request")
			t.Run("now", func(t *T) {
				t.Msg("byebye if Is(time.Now(), night) else hello")
			})
			t.Msg("response")
		})
		t.Run("world", func(t *T) {
			t.Msg("Println(\"yay\")")
		})
	})
}
