package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

// Pool: define by hand
type Pool struct {
	user *User
}

// User: define by hand
func (p *Pool) User() User {
	return *p.user
}

// NewSuite: define by hand
func NewSuite(t *testing.T) (*Suite, func()) {
	user := &User{Name: "foo"}
	return &Suite{
		t: t,
		Pool: &Pool{
			user: user,
		},
	}, func() {}
}

// Suite: generated
type Suite struct {
	t *testing.T
	*Pool
}

// renderWith: generated?
func (s *Suite) renderWith(t *testing.T, use func(t *testing.T, n *Notificator)) {
	t.Helper()
	n := &Notificator{Client: &FakeClient{}}
	use(t, n)
	AssertWithBox(t, n, func(box Box) {
		t.Helper()
		if box.Empty() {
			t.Fatal("unexpected empty")
			return
		}

		wpath := filepath.Join("testdata", t.Name())
		os.MkdirAll(filepath.Dir(wpath), 0744)

		t.Logf("write: %s", wpath)
		f, err := os.Create(wpath)
		if err != nil {
			t.Fatalf("!%+v", err)
		}

		fmt.Fprintf(f, "channel: %s\n", box[0].Channel)
		fmt.Fprintln(f, box[0].Text)
		defer f.Close()
	})
}

// RenderRegistered: generated
func (s *Suite) RenderRegistered(t *testing.T) {
	t.Helper()
	s.renderWith(t, func(t *testing.T, n *Notificator) {
		ctx := context.Background()
		user := s.User()
		if err := n.NotifyRegistered(ctx, user); err != nil {
			t.Fatal(err)
		}
	})
}

// RenderCancelled: generated
func (s *Suite) RenderCancelled(t *testing.T) {
	t.Helper()
	s.renderWith(t, func(t *testing.T, n *Notificator) {
		ctx := context.Background()
		user := s.User()
		if err := n.NotifyCancelled(ctx, user); err != nil {
			t.Fatal(err)
		}
	})
}

// TestRender: generated
func TestRender(t *testing.T) {
	s, teardown := NewSuite(t)
	defer teardown()

	t.Run("registered", s.RenderRegistered)
	t.Run("cancelled", s.RenderCancelled)
}
