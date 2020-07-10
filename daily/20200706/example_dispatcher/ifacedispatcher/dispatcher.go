package dispatcher

import (
	"fmt"
	"m/mail"
	"m/slack"
)

type Dispatcher interface {
	UserCreated(name string)
}

func NewDispatcher(
	slackClient *slack.Client,
	mailClient *mail.Client,
) Dispatcher {
	return &dispatcher{
		Slack: slackClient,
		Mail:  mailClient,
	}
}

type dispatcher struct {
	Slack *slack.Client
	Mail  *mail.Client
}

func (d *dispatcher) UserCreated(name string) {
	fmt.Println("UserCreated")
	d.Slack.PostMessage(fmt.Sprintf("created user %s", name))
	d.Mail.SendMail("login url = https://example.net/login?xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
}
