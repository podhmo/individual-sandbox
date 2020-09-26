package main

import (
	"context"
	"m/06separated/slackclient"
	"m/06separated/slackdriver/slacktest"
	"testing"
)

func TestForHello(t *testing.T) {
	client := slackclient.New(slacktest.NewDriver())
	ctx := context.Background()

	err := client.SendHelloMessage(ctx)
	if err != nil {
		t.Fatalf("!! %+v", err)
	}

	slacktest.AssertWithBox(t, client,
		func(t *testing.T, box slacktest.Box) {
			t.Logf("channel:%s, text:%q", box[0].Channel, box[0].Text)
		},
	)
}

func TestForBybye(t *testing.T) {
	client := slackclient.New(slacktest.NewDriver())
	ctx := context.Background()

	err := client.SendByebyeMessage(ctx)
	if err != nil {
		t.Fatalf("!! %+v", err)
	}

	slacktest.AssertWithBox(t, client,
		func(t *testing.T, box slacktest.Box) {
			t.Logf("channel:%s, text:%q", box[0].Channel, box[0].Text)
		},
	)
}
