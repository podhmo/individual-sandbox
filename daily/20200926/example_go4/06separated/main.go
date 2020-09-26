package main

import (
	"context"
	"log"
	"m/06separated/slackclient"
	"m/06separated/slackdriver/slackapi"
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

	client := slackclient.New(
		slackapi.NewDriver(slack.New(token, slack.OptionDebug(debug))),
	)

	if err := client.SendHelloMessage(context.Background()); err != nil {
		log.Printf("!! %+v", err)
	}
}
