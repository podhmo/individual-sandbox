package yyy

import (
	"fmt"
	"m/mail"
)

type YYYDispatcher struct {
	Mail *mail.Client
}

func (d *YYYDispatcher) OnYYY(name string) {
	d.Mail.SendMail(fmt.Sprintf("yyy %s", name))
}

func NewDispatcher(client *mail.Client) *YYYDispatcher {
	return &YYYDispatcher{
		Mail: client,
	}
}
