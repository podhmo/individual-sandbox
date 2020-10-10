package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"m/mq"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/sqs"
	"golang.org/x/xerrors"
)

type Client struct {
	Driver mq.Driver
}

type PingMessage struct {
	Text      string    `json:"text"`
	CreatedAt time.Time `json:"createdAt"`

	raw interface{} `json:"-"`
}

func (c *Client) SendPing(ctx context.Context, m PingMessage) error {
	b, err := json.Marshal(m)
	if err != nil {
		return xerrors.Errorf("json encode: %w", err)
	}
	return c.Driver.SendMessage(ctx, mq.QueueInfo{Type: "ping"}, []mq.Message{{Body: string(b)}})
}

func (c *Client) RecivePing(ctx context.Context) ([]PingMessage, func(PingMessage) error, error) {
	raws, ack, err := c.Driver.ReciveMessage(ctx, mq.QueueInfo{Type: "ping"})
	if err != nil {
		return nil, nil, xerrors.Errorf("recv message: %w", err)
	}

	r := make([]PingMessage, 0, len(raws))
	for i, raw := range raws {
		var m PingMessage
		if err := json.Unmarshal([]byte(raw.Body), &m); err != nil {
			log.Printf("json decode error: [%d]: %+v", i, err)
			ack(raws[i]) // 謎なものはdumpしてしまう?
			continue
			// return nil, nil, xerrors.Errorf("json decode: %w", err)
		}
		m.raw = raw
		r = append(r, m)
	}
	return r, func(m PingMessage) error {
		return ack(m.raw)
	}, nil
}

func main() {
	// snippet-start:[sqs.go.receive_messages.args]
	queue := flag.String("q", "", "The name of the queue")
	flag.Parse()
	if *queue == "" {
		fmt.Println("You must supply the name of a queue (-q QUEUE)")
		return
	}

	if err := run(*queue); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(queueName string) error {
	var c *Client
	// c = &Client{Driver: &FakeDriver{W: os.Stdout, BoxMap: map[string][]Message{}}}

	{
		lv := aws.LogDebug
		cfg := aws.Config{
			CredentialsChainVerboseErrors: aws.Bool(true),
			LogLevel:                      &lv,
		}

		// Create a session that gets credential values from ~/.aws/credentials
		// and the default region from ~/.aws/config
		// snippet-start:[sqs.go.receive_messages.sess]
		sess := session.Must(session.NewSessionWithOptions(session.Options{
			SharedConfigState: session.SharedConfigEnable,
			Config:            cfg,
		}))
		svc := sqs.New(sess)
		c = &Client{
			Driver: &mq.SQSDriver{
				Service: svc,
				ResolveURL: func(info mq.QueueInfo) (string, error) {
					o, err := svc.GetQueueUrl(&sqs.GetQueueUrlInput{
						QueueName: aws.String(queueName),
					})
					if err != nil {
						return "", xerrors.Errorf("get queue url: %w", err)
					}
					return *o.QueueUrl, nil
				},
			},
		}
	}
	ctx := context.Background()

	if err := c.SendPing(ctx, PingMessage{Text: "hello", CreatedAt: time.Now()}); err != nil {
		return xerrors.Errorf("send ping: %w", err)
	}
	if err := c.SendPing(ctx, PingMessage{Text: "byebye", CreatedAt: time.Now()}); err != nil {
		return xerrors.Errorf("send ping: %w", err)
	}

	for i := 0; i < 2; i++ {
		ms, ack, err := c.RecivePing(ctx)
		if err != nil {
			return xerrors.Errorf("recv ping: %w", err)
		}
		for _, m := range ms {
			fmt.Println("got", m)
			ack(m)
		}
	}
	return nil
}
