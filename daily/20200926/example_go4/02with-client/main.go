package main

import (
	"context"
	"log"
	"os"
	"strconv"

	"github.com/slack-go/slack"
	"golang.org/x/xerrors"
)

type Client struct {
	API *slack.Client
}

func (c *Client) SendHelloMessage(ctx context.Context) error {
	if _, _, _, err := c.API.SendMessageContext(
		ctx, "#random", slack.MsgOptionText(`
hello, this is test
`, true)); err != nil {
		return xerrors.Errorf("send message: %w", err)
	}
	return nil
}

func main() {
	token := os.Getenv("SLACK_TOKEN")
	if token == "" {
		log.Fatal("slack token is not set")
	}
	debug, _ := strconv.ParseBool(os.Getenv("DEBUG"))

	client := &Client{API: slack.New(token, slack.OptionDebug(debug))}

	if err := client.SendHelloMessage(context.Background()); err != nil {
		log.Printf("!! %+v", err)
	}
}
