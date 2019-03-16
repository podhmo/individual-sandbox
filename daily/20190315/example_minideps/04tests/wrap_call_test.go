package minideps_test

import (
	"context"
	minideps "m/minideps2"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestWrapCall(t *testing.T) {
	t.Run("context", func(t *testing.T) {
		type k string

		const (
			k0 = k("")
		)

		g, run := minideps.New()

		g.NewNode("x", func(s minideps.State) {
			v := s.Context().Value(k0)
			assert.EqualValues(t, 100, v)
		})
		value := 100
		ctx := context.WithValue(context.Background(), k0, value)
		run(minideps.WithContext(ctx))
	})

	t.Run("wrap", func(t *testing.T) {
		r := []string{}
		g, run := minideps.New()

		g.NewNode("x", func(s minideps.State) {
			r = append(r, s.Name)
		})
		g.NewNode("y", func(s minideps.State) {
			r = append(r, s.Name)
		})

		run(minideps.WithWrapFunction(func(s minideps.State, next func(minideps.State)) {
			r = append(r, "b")
			defer func() {
				r = append(r, "a")
			}()
			next(s)
		}))
		assert.Exactly(t, []string{"b", "x", "a", "b", "y", "a"}, r)

	})
}
