package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

func TestRender(t *testing.T) {
	user := User{Name: "foo"}

	renderWith := func(t *testing.T, use func(t *testing.T, n *Notificator)) {
		n := &Notificator{Client: &FakeClient{}}
		use(t, n)
		AssertWithBox(t, n, func(box Box) {
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

	t.Run("registered", func(t *testing.T) {
		renderWith(t, func(t *testing.T, n *Notificator) {
			ctx := context.Background()
			n.NotifyRegistered(ctx, user)
		})
	})
}
