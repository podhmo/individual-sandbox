package main

import (
	dispatcher "m/ifacedispatcher"
	"m/mail"
	"m/slack"
)

func main() {
	d := dispatcher.NewDispatcher(
		slack.NewClient(),
		mail.NewClient(),
	)

	d.UserCreated("foo")
}
