package minideps_test

import (
	minideps "m/minideps2"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDeps(t *testing.T) {
	candidates := []struct {
		msg      string
		Setup    func(g *minideps.Graph) func() []string
		Expected []string
	}{
		{
			msg: "x",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					calls = append(calls, s.Name)
				}

				g.NewNode("x", use)
				return func() []string { return calls }
			},
			Expected: []string{"x"},
		},
		{
			msg: "x -> y",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					calls = append(calls, s.Name)
				}

				x := g.NewNode("x", use)
				g.NewNode("y", use, g.WithDepends(x))
				return func() []string { return calls }
			},
			Expected: []string{"x", "y"},
		},
		{
			msg: "{x0, x1} -> y",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					calls = append(calls, s.Name)
				}

				x0 := g.NewNode("x0", use)
				x1 := g.NewNode("x1", use)
				g.NewNode("y", use, g.WithDepends(x0, x1))
				return func() []string { return calls }
			},
			Expected: []string{"x0", "x1", "y"},
		},
		{
			msg: "x -> {y0, y1}",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					calls = append(calls, s.Name)
				}

				x := g.NewNode("x", use)
				g.NewNode("y0", use, g.WithDepends(x))
				g.NewNode("y1", use, g.WithDepends(x))
				return func() []string { return calls }
			},
			Expected: []string{"x", "y0", "y1"},
		},
		{
			msg: "x -> y -> z",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					if s.Disabled {
						return
					}
					calls = append(calls, s.Name)
				}

				x := g.NewNode("x", use)
				y := g.NewNode("y", use, g.WithDepends(x))
				z := g.NewNode("z", use, g.WithDepends(y))
				z.Adjust()
				return func() []string { return calls }
			},
			Expected: []string{"x", "y", "z"},
		},
		{
			msg: "x -> y -> z; disabled z",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					if s.Disabled {
						return
					}
					calls = append(calls, s.Name)
				}

				x := g.NewNode("x", use)
				y := g.NewNode("y", use, g.WithDepends(x))
				z := g.NewNode("z", use, g.WithDepends(y))
				z.Adjust(g.WithDisabled())
				return func() []string { return calls }
			},
			Expected: []string{},
		},
		{
			msg: "{x0, x1} -> y0; {x1, x2} -> y1",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					calls = append(calls, s.Name)
				}

				x0 := g.NewNode("x0", use)
				x1 := g.NewNode("x1", use)
				x2 := g.NewNode("x2", use)
				g.NewNode("y0", use, g.WithDepends(x0, x1))
				g.NewNode("y1", use, g.WithDepends(x1, x2))
				return func() []string { return calls }
			},
			Expected: []string{"x0", "x1", "y0", "x2", "y1"},
		},
		{
			msg: "{x0, x1} -> y0; {x1, x2} -> y1; disable y1",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					if s.Disabled {
						return
					}
					calls = append(calls, s.Name)
				}

				x0 := g.NewNode("x0", use)
				x1 := g.NewNode("x1", use)
				x2 := g.NewNode("x2", use)
				y0 := g.NewNode("y0", use, g.WithDepends(x0, x1))
				y1 := g.NewNode("y1", use, g.WithDepends(x1, x2))

				y0.Adjust()
				y1.Adjust(g.WithDisabled())
				return func() []string { return calls }
			},
			Expected: []string{"x0", "x1", "y0"},
		},
		{
			msg: "{x0, x1} -> y0; {x1, x2} -> y1; disable y1; disable y0",
			Setup: func(g *minideps.Graph) func() []string {
				calls := []string{}
				use := func(s minideps.State) {
					if s.Disabled {
						return
					}
					calls = append(calls, s.Name)
				}

				x0 := g.NewNode("x0", use)
				x1 := g.NewNode("x1", use)
				x2 := g.NewNode("x2", use)
				y0 := g.NewNode("y0", use, g.WithDepends(x0, x1))
				y1 := g.NewNode("y1", use, g.WithDepends(x1, x2))

				y0.Adjust(g.WithDisabled())
				y1.Adjust(g.WithDisabled())
				return func() []string { return calls }
			},
			Expected: []string{},
		},
	}
	for _, c := range candidates {
		t.Run(c.msg, func(t *testing.T) {
			g, start := minideps.New()
			calls := c.Setup(g)
			start()
			assert.Exactly(t, c.Expected, calls())
		})
	}
}
