package main

import (
	"context"
	"log"
	"os"
	"strconv"

	"github.com/slack-go/slack"
	"golang.org/x/xerrors"
)

type Driver interface {
	SendMessage(ctx context.Context, channel, text string) error
}

type SlackDriver struct {
	API *slack.Client
}

func (d *SlackDriver) SendMessage(ctx context.Context, channel, text string) error {
	if _, _, _, err := d.API.SendMessageContext(
		ctx, channel, slack.MsgOptionText(text, true)); err != nil {
		return xerrors.Errorf("send message: %w", err)
	}
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

	client := &Client{Driver: &SlackDriver{API: slack.New(token, slack.OptionDebug(debug))}}

	if err := client.SendHelloMessage(context.Background()); err != nil {
		log.Printf("!!\n%+v", err)
	}
}
