package main

import (
	"context"
	"log"
	"os"
	"strconv"
	"strings"

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

func main() {
	token := os.Getenv("SLACK_TOKEN")
	if token == "" {
		log.Fatal("slack token is not set")
	}
	debug, _ := strconv.ParseBool(os.Getenv("DEBUG"))

	client := &Client{Driver: &FakeDriver{Echo: debug}}
	if err := client.SendHelloMessage(context.Background()); err != nil {
		log.Printf("!!\n%+v", err)
	}
}
