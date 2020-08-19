package main

import (
	"context"
	"testing"
)

func TestIt(t *testing.T) {
	user := User{Name: "foo"}

	n := &Notificator{Client: &FakeClient{}}
	ctx := context.Background()
	n.NotifyRegistered(ctx, user)
	AssertWithBox(t, n, func(box Box) {
		if box.Empty() {
			t.Errorf("unexpected len(n) is %d", len(box))
		}
		t.Logf("channel=%q, text=%q", box[0].Channel, box[0].Text)
	})
}
