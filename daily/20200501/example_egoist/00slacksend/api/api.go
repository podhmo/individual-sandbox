package api

import (
	"context"
	"fmt"
	"log"
	"os"

	"m/config"

	"github.com/pkg/errors"
	"github.com/slack-go/slack"
)

type Client = slack.Client

func NewClient(c *config.Config) (*Client, error) {
	if c.Slack.APIToken == "" {
		return nil, fmt.Errorf("access token is not found")
	}
	return slack.New(
		c.Slack.APIToken,
		slack.OptionDebug(true),
		slack.OptionLog(log.New(os.Stderr, "", 0)),
	), nil
}

type API struct {
	Client *Client
}

func NewAPI(client *Client) *API {
	return &API{
		Client: client,
	}
}

// https://github.com/slack-go/slack/blob/master/examples/messages/messages.go

func (api *API) Send(ctx context.Context, message string, channelID string) error {
	attachment := slack.Attachment{
		Pretext: "some pretext",
		Text:    "some text",
		// Uncomment the following part to send a field too
		/*
			Fields: []slack.AttachmentField{
				slack.AttachmentField{
					Title: "a",
					Value: "no",
				},
			},
		*/
	}

	noEscape := false
	channelID, timestamp, err := api.Client.PostMessage(
		channelID,
		slack.MsgOptionText(message, noEscape),
		slack.MsgOptionAttachments(attachment),
	)
	if err != nil {
		return errors.Wrap(err, "send")
	}

	// todo: log
	fmt.Fprintf(os.Stderr, "Message successfully sent to channel %s at %s\n", channelID, timestamp)
	return nil
}
