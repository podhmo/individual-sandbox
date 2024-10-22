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
}

func (t *T) Msg(msg string) {
	t.stack[len(t.stack)-1] = append(t.stack[len(t.stack)-1], msg)
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
		if len(last) > 0 {
			fmt.Fprintf(w, "%s%s;\n", strings.Repeat("  ", t.lv), strings.Join(last, " --> "))
		}
		t.stack = t.stack[:len(t.stack)-1]
	}
	t.lv--
}

func Do(fn func(t *T)) {
	t := &T{stack: [][]string{{}}}
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

func main() {
	Do(func(t *T) {
		t.Run("hello", func(t *T) {
			t.Msg("request")
			t.Run("now", func(t *T) {
				t.Msg("byebye if Is(time.Now(), night) else hello")
			})
			t.Msg("response")
		})
		t.Run("world", func(t *T) {
			t.Msg("yay")
		})
	})
}
