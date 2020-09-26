package slackclient

import (
	"context"
	"m/06separated/slackclient/slackdriver"

	"golang.org/x/xerrors"
)

type Client struct {
	*slackdriver.Client
}

func New(driver slackdriver.Driver) *Client {
	return &Client{Client: slackdriver.NewClient(driver)}
}

func (c *Client) SendHelloMessage(ctx context.Context) error {
	if err := c.Driver().SendMessage(ctx, "#random", `
hello, this is test.
`); err != nil {
		return xerrors.Errorf("send: %w", err)
	}
	return nil
}

func (c *Client) SendByebyeMessage(ctx context.Context) error {
	if err := c.Driver().SendMessage(ctx, "#randomm", `
byebye, this is test.
`); err != nil {
		return xerrors.Errorf("send: %w", err)
	}
	return nil
}
