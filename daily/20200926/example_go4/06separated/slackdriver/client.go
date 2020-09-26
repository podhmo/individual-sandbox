package slackdriver

import "context"

type Driver interface {
	SendMessage(ctx context.Context, channel, text string) error
}

type Client struct {
	driver Driver
}

func (c *Client) Driver() Driver {
	return c.driver
}
func NewClient(driver Driver) *Client {
	return &Client{driver: driver}
}
