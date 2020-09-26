package main

import (
	"context"
	"log"
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
	api := slack.New(token, slack.OptionDebug(debug))

	// func MsgOptionSchedule(postAt string) MsgOption {
	// func MsgOptionPost() MsgOption {
	// func MsgOptionPostEphemeral(userID string) MsgOption {
	// func MsgOptionMeMessage() MsgOption {
	// func MsgOptionUpdate(timestamp string) MsgOption {
	// func MsgOptionDelete(timestamp string) MsgOption {
	// func MsgOptionUnfurl(timestamp string, unfurls map[string]Attachment) MsgOption {
	// func MsgOptionResponseURL(url string, responseType string) MsgOption {
	// func MsgOptionReplaceOriginal(responseURL string) MsgOption {
	// func MsgOptionDeleteOriginal(responseURL string) MsgOption {
	// func MsgOptionAsUser(b bool) MsgOption {
	// func MsgOptionUser(userID string) MsgOption {
	// func MsgOptionUsername(username string) MsgOption {
	// func MsgOptionText(text string, escape bool) MsgOption {
	// func MsgOptionAttachments(attachments ...Attachment) MsgOption {
	// func MsgOptionBlocks(blocks ...Block) MsgOption {
	// func MsgOptionEnableLinkUnfurl() MsgOption {
	// func MsgOptionDisableLinkUnfurl() MsgOption {
	// func MsgOptionDisableMediaUnfurl() MsgOption {
	// func MsgOptionDisableMarkdown() MsgOption {
	// func MsgOptionTS(ts string) MsgOption {
	// func MsgOptionBroadcast() MsgOption {
	// func MsgOptionCompose(options ...MsgOption) MsgOption {
	// func MsgOptionParse(b bool) MsgOption {
	// func MsgOptionIconURL(iconURL string) MsgOption {
	// func MsgOptionIconEmoji(iconEmoji string) MsgOption {
	// func MsgOptionPostMessageParameters(params PostMessageParameters) MsgOption {

	if _, _, _, err := api.SendMessageContext(context.Background(), "#random", slack.MsgOptionText(`
hello, this is test
`, true)); err != nil {
		log.Printf("!! %+v", err)
	}
}
