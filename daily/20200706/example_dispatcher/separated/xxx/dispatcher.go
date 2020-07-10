package xxx

import (
	"fmt"
	"m/slack"
)

type XXXDispatcher struct {
	Slack *slack.Client
}

func (d *XXXDispatcher) OnXXX(name string) {
	d.Slack.PostMessage(fmt.Sprintf("xxx %s", name))
}

func NewDispatcher(client *slack.Client) *XXXDispatcher {
	return &XXXDispatcher{
		Slack: client,
	}
}
