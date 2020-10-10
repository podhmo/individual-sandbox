package mq

import (
	"context"
	"fmt"
	"io"
	"log"

	"github.com/aws/aws-sdk-go/aws"
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

type CleanupFunction = func(interface{}) error
type Driver interface {
	SendMessage(ctx context.Context, info QueueInfo, m []Message) error
	ReciveMessage(ctx context.Context, info QueueInfo) ([]Message, CleanupFunction, error)
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
func (d *FakeDriver) ReciveMessage(ctx context.Context, info QueueInfo) ([]Message, CleanupFunction, error) {
	m := d.BoxMap[info.Type][0]
	d.BoxMap[info.Type] = d.BoxMap[info.Type][1:]
	fmt.Fprintln(d.W, "	recv:", m.Body)
	return []Message{m}, func(interface{}) error { return nil }, nil
}

type SQSDriver struct {
	Service    *sqs.SQS
	ResolveURL func(QueueInfo) (string, error) // TODO: cache?
}

func (d *SQSDriver) SendMessage(ctx context.Context, info QueueInfo, ms []Message) error {
	queueURL, err := d.ResolveURL(info)
	if err != nil {
		return xerrors.Errorf("resolve url: %w", err)
	}

	// TODO: use batch operation
	for i := range ms {
		_, err := d.Service.SendMessageWithContext(ctx, &sqs.SendMessageInput{
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

func (d *SQSDriver) ReciveMessage(ctx context.Context, info QueueInfo) ([]Message, CleanupFunction, error) {
	queueURL, err := d.ResolveURL(info)
	if err != nil {
		return nil, nil, xerrors.Errorf("resolve url: %w", err)
	}

	msgResult, err := d.Service.ReceiveMessageWithContext(ctx, &sqs.ReceiveMessageInput{
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
		if _, err := d.Service.DeleteMessage(&sqs.DeleteMessageInput{
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
