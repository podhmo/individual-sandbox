package main

import (
	"crypto/sha1"
	"fmt"
	"strings"
	"testing"
)

func TestCommit(t *testing.T) {
	lines := func(xs ...string) []string { return xs }
	cases := []struct {
		prev []string // "<name>@<content>"
		cur  []string // "<name>@<content>"
		want []string // "<action>:<name>"
	}{
		{prev: nil, cur: nil, want: nil},
		{prev: nil, cur: lines("hello@"), want: lines("C:hello")},                            // create
		{prev: lines("hello@1"), cur: lines("hello@1"), want: lines("-:hello")},              // not modified
		{prev: lines("hello@1"), cur: lines("hello@2"), want: lines("U:hello")},              // update
		{prev: lines("hello@"), cur: nil, want: lines("D:hello")},                            // deleted
		{prev: lines("hello@1"), cur: lines("hello2@2"), want: lines("C:hello2", "D:hello")}, // deleted (renamed)
	}

	parseEntry := func(m *manager, x string) entry {
		parts := strings.SplitN(x, "@", 2)
		name := parts[0]
		content := []byte(parts[1])
		return m.NewEntry(name, content)
	}

	for i, c := range cases {
		c := c
		t.Run(fmt.Sprintf("case%d", i), func(t *testing.T) {
			var prev *history
			if len(c.prev) > 0 {
				m := &manager{hash: sha1.New()}
				for _, x := range c.prev {
					parseEntry(m, x)
				}
				prev = &m.current
				prev.usedCount = make([]int, len(prev.entries))
			}

			m := &manager{hash: sha1.New()}
			for _, x := range c.cur {
				parseEntry(m, x)
			}
			actions, err := m.CommitWith(prev)
			if err != nil {
				t.Errorf("unexpected error %+v", err)
				return
			}
			got := make([]string, len(actions))
			for i, ac := range actions {
				got[i] = fmt.Sprintf("%s:%s", ac.Type, ac.Name())
			}
			if want, got := strings.TrimSpace(strings.Join(c.want, "\n\t")), strings.TrimSpace(strings.Join(got, "\n\t")); want != got {
				t.Errorf("want:\n\t%s\nbut got:\n\t%s", want, got)
			}
		})
	}
}
