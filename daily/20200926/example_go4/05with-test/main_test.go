package main

import (
	"context"
	"log"
	"strings"
	"testing"

	"golang.org/x/xerrors"
)

type Driver interface {
	SendMessage(ctx context.Context, channel, text string) error
}

type Message struct {
	Channel string
	Text    string
}
type Box []Message
type FakeDriver struct {
	Echo bool
	Box  Box
}

func (d *FakeDriver) SendMessage(ctx context.Context, channel, text string) error {
	m := Message{
		Channel: channel,
		Text:    text,
	}
	if d.Echo {
		log.Printf("send message (%s): %s", m.Channel, strings.TrimRight(strings.ReplaceAll(m.Text, "\n", "\n\t"), "\n\t"))
	}
	d.Box = append(d.Box, m)
	return nil
}

type Client struct {
	Driver Driver
}

func (c *Client) SendHelloMessage(ctx context.Context) error {
	if err := c.Driver.SendMessage(ctx, "#randomm", `
hello, this is test.
`); err != nil {
		return xerrors.Errorf("send: %w", err)
	}
	return nil
}

func AssertWithBox(t *testing.T, client *Client, assert func(t *testing.T, box Box)) {
	t.Helper()
	driver, ok := client.Driver.(*FakeDriver)
	if !ok {
		t.Fatalf("invalid type: %T", client.Driver)
	}

	box := driver.Box
	t.Logf("box length %v", len(box))
	if len(box) == 0 {
		t.Fatalf("expect not empty")
	}

	assert(t, box)
}

func AssertWithBoxNoError(t *testing.T,
	client *Client,
	err error,
	assert func(t *testing.T, box Box),
) {
	t.Helper()
	if err != nil {
		t.Fatalf("!! %+v", err)
	}
	AssertWithBox(t, client, assert)
}

func NewTestClient(t *testing.T) *Client {
	return &Client{
		Driver: &FakeDriver{Echo: false},
	}
}

func TestIt(t *testing.T) {
	client := NewTestClient(t)
	ctx := context.Background()
	AssertWithBoxNoError(t, client,
		client.SendHelloMessage(ctx),
		func(t *testing.T, box Box) {
			t.Logf("channel:%s, text:%q", box[0].Channel, box[0].Text)
		},
	)
}
