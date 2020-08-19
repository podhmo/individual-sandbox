package send

import (
	"context"
	"m/model"
	"testing"
)

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

type Box []Message

func (b Box) Empty() bool {
	return len(b) == 0
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

// NewSuite ...
func NewSuite(t *testing.T) (*Suite, func()) {
	return &Suite{
		Pool: &Pool{
			user: &model.User{Name: "foo"},
		},
	}, func() {}
}

// Pool ...
type Pool struct {
	user *model.User
}

func (p *Pool) User() model.User {
	return *p.user
}
func (p *Pool) X() X {
	return X{Name: "X"}
}
