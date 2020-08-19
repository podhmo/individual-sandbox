package main

import "context"

type Client interface {
	Send(ctx context.Context, channel string, text string) error
}

type Message struct {
	Channel string
	Text    string
}

type client struct{}

func (c *client) Send(ctx context.Context, channel string, text string) error {
	panic("implement")
	return nil
}
