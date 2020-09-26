package slackapi

import (
	"context"

	"github.com/slack-go/slack"
	"golang.org/x/xerrors"
)

func NewDriver(api *slack.Client) *Driver {
	return &Driver{
		API: api,
	}
}

type Driver struct {
	API *slack.Client
}

func (d *Driver) SendMessage(ctx context.Context, channel, text string) error {
	if _, _, _, err := d.API.SendMessageContext(
		ctx, channel, slack.MsgOptionText(text, true)); err != nil {
		return xerrors.Errorf("send message: %w", err)
	}
	return nil
}
