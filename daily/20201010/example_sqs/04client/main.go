package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/sqs"
	"golang.org/x/xerrors"
)

type QueueInfo struct {
	Type string
}

type Message struct {
	GroupID string
	Body    string

	raw interface{} `json:"-"`
}

type Driver interface {
	SendMessage(ctx context.Context, info QueueInfo, m []Message) error
	ReciveMessage(ctx context.Context, info QueueInfo) ([]Message, func(interface{}) error, error)
}

type Client struct {
	Driver Driver
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
	return c.Driver.SendMessage(ctx, QueueInfo{Type: "ping"}, []Message{{Body: string(b)}})
}

func (c *Client) RecivePing(ctx context.Context) ([]PingMessage, func(PingMessage) error, error) {
	raws, ack, err := c.Driver.ReciveMessage(ctx, QueueInfo{Type: "ping"})
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

type FakeDriver struct {
	W      io.Writer
	BoxMap map[string][]Message
}

func (d *FakeDriver) SendMessage(ctx context.Context, info QueueInfo, ms []Message) error {
	for _, m := range ms {
		fmt.Fprintln(d.W, "	send:", m.Body)
		d.BoxMap[info.Type] = append(d.BoxMap[info.Type], m)
	}
	return nil
}
func (d *FakeDriver) ReciveMessage(ctx context.Context, info QueueInfo) ([]Message, func(interface{}) error, error) {
	m := d.BoxMap[info.Type][0]
	d.BoxMap[info.Type] = d.BoxMap[info.Type][1:]
	fmt.Fprintln(d.W, "	recv:", m.Body)
	return []Message{m}, func(interface{}) error { return nil }, nil
}

type SQSDriver struct {
	svc        *sqs.SQS
	resolveURL func(QueueInfo) (string, error) // TODO: cache?
}

func (d *SQSDriver) SendMessage(ctx context.Context, info QueueInfo, ms []Message) error {
	queueURL, err := d.resolveURL(info)
	if err != nil {
		return xerrors.Errorf("resolve url: %w", err)
	}

	// TODO: ack batch operation
	for i := range ms {
		_, err := d.svc.SendMessageWithContext(ctx, &sqs.SendMessageInput{
			// DelaySeconds: aws.Int64(10),
			MessageAttributes: map[string]*sqs.MessageAttributeValue{
				"Title": &sqs.MessageAttributeValue{
					DataType:    aws.String("String"),
					StringValue: aws.String("The Whistler"),
				},
			},
			MessageBody:    &ms[i].Body,
			MessageGroupId: aws.String("default"), // hmm..
			QueueUrl:       &queueURL,
		})
		if err != nil {
			return xerrors.Errorf("send message: [%d]: %w", i, err)
		}
	}
	return nil
}

func (d *SQSDriver) ReciveMessage(ctx context.Context, info QueueInfo) ([]Message, func(interface{}) error, error) {
	queueURL, err := d.resolveURL(info)
	if err != nil {
		return nil, nil, xerrors.Errorf("resolve url: %w", err)
	}

	msgResult, err := d.svc.ReceiveMessageWithContext(ctx, &sqs.ReceiveMessageInput{
		AttributeNames: []*string{
			aws.String(sqs.MessageSystemAttributeNameSentTimestamp),
		},
		MessageAttributeNames: []*string{
			aws.String(sqs.QueueAttributeNameAll),
		},
		QueueUrl:            &queueURL,
		MaxNumberOfMessages: aws.Int64(10), // 普通は多めに
		WaitTimeSeconds:     aws.Int64(5),  // 普通は長めに
		VisibilityTimeout:   aws.Int64(5),  // 普通はどれくらいだろう
	})
	if err != nil {
		return nil, nil, xerrors.Errorf("receive message: %w", err)
	}

	r := make([]Message, len(msgResult.Messages))
	for i := range msgResult.Messages {
		r[i] = Message{
			Body: *msgResult.Messages[i].Body,
			raw:  msgResult.Messages[i],
		}
	}

	deleteMessage := func(ob interface{}) error {
		m, ok := ob.(Message)
		if !ok {
			log.Printf("!! invalid message type: %T", m)
			return fmt.Errorf("!! invalid message type: %T", m)
		}
		raw, ok := m.raw.(*sqs.Message)
		if !ok {
			log.Printf("!! invalid raw message type: %T", raw)
			return fmt.Errorf("!! invalid raw message type: %T", raw)
		}
		if _, err := d.svc.DeleteMessage(&sqs.DeleteMessageInput{
			QueueUrl:      &queueURL,
			ReceiptHandle: raw.ReceiptHandle,
		}); err != nil {
			log.Printf("!! delete message: %w", err)
			return xerrors.Errorf("!! delete message: %w", err)
		}
		return nil
	}
	return r, deleteMessage, nil
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
			Driver: &SQSDriver{
				svc: svc,
				resolveURL: func(info QueueInfo) (string, error) {
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
