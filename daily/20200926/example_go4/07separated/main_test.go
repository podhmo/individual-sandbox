package main

import (
	"context"
	"m/07separated/notification"
	"m/07separated/slackclient/slacktest"
	"testing"
)

func TestForHello(t *testing.T) {
	n := notification.New(slacktest.NewDriver())
	ctx := context.Background()

	err := n.NotifyHelloMessage(ctx)
	if err != nil {
		t.Fatalf("!! %+v", err)
	}

	slacktest.AssertWithBox(t, n,
		func(t *testing.T, box slacktest.Box) {
			t.Logf("channel:%s, text:%q", box[0].Channel, box[0].Text)
		},
	)
}

func TestForBybye(t *testing.T) {
	n := notification.New(slacktest.NewDriver())
	ctx := context.Background()

	err := n.NotifyByebyeMessage(ctx)
	if err != nil {
		t.Fatalf("!! %+v", err)
	}

	slacktest.AssertWithBox(t, n,
		func(t *testing.T, box slacktest.Box) {
			t.Logf("channel:%s, text:%q", box[0].Channel, box[0].Text)
		},
	)
}
