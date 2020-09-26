package main

import (
	"context"
	"log"
	"m/07separated/notification"
	"m/07separated/slackclient/slackapi"
	"os"
	"strconv"

	"github.com/slack-go/slack"
)

func main() {
	token := os.Getenv("SLACK_TOKEN")
	if token == "" {
		log.Fatal("slack token is not set")
	}
	debug, _ := strconv.ParseBool(os.Getenv("DEBUG"))

	n := notification.New(
		slackapi.NewDriver(slack.New(token, slack.OptionDebug(debug))),
	)

	if err := n.NotifyHelloMessage(context.Background()); err != nil {
		log.Printf("!! %+v", err)
	}
}
