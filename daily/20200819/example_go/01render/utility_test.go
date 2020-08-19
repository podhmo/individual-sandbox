package main

import (
	"context"
	"testing"
)

type Box []Message

func (b Box) Empty() bool {
	return len(b) == 0
}

type FakeClient struct {
	Box Box
}

func (c *FakeClient) Send(ctx context.Context, channel string, text string) error {
	c.Box = append(c.Box, Message{
		Channel: channel,
		Text:    text,
	})
	return nil
}

func AssertWithBox(t *testing.T, n *Notificator, assert func(box Box)) {
	t.Helper()
	c, ok := n.Client.(*FakeClient)
	if !ok {
		t.Fatalf("%T is not FakeClient", n.Client)
		return
	}
	assert(c.Box)
}
