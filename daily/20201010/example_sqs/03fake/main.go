package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"time"

	"golang.org/x/xerrors"
)

type QueueInfo struct {
	Type string
}

type Message struct {
	QueueInfo

	GroupID string
	Body    string
}

type CleanupFunction = func(interface{}) error
type Driver interface {
	SendMessage(ctx context.Context, m []Message) error
	ReciveMessage(ctx context.Context, info QueueInfo) ([]Message, CleanupFunction, error)
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
	raw := Message{Body: string(b), QueueInfo: QueueInfo{Type: "ping"}}
	return c.Driver.SendMessage(ctx, []Message{raw})
}

func (c *Client) RecivePing(ctx context.Context) ([]PingMessage, func(PingMessage) error, error) {
	raws, cleanup, err := c.Driver.ReciveMessage(ctx, QueueInfo{Type: "ping"})
	if err != nil {
		return nil, nil, xerrors.Errorf("recv message: %w", err)
	}
	r := make([]PingMessage, len(raws))
	for i, raw := range raws {
		var m PingMessage
		if err := json.Unmarshal([]byte(raw.Body), &m); err != nil {
			return nil, nil, xerrors.Errorf("json decode: %w", err)
		}
		m.raw = raw
		r[i] = m
	}
	return r, func(m PingMessage) error {
		return cleanup(m.raw)
	}, nil
}

type FakeDriver struct {
	W      io.Writer
	BoxMap map[string][]Message
}

func (d *FakeDriver) SendMessage(ctx context.Context, ms []Message) error {
	for _, m := range ms {
		fmt.Fprintln(d.W, "	send:", m.Body)
		d.BoxMap[m.Type] = append(d.BoxMap[m.Type], m)
	}
	return nil
}
func (d *FakeDriver) ReciveMessage(ctx context.Context, info QueueInfo) ([]Message, CleanupFunction, error) {
	m := d.BoxMap[info.Type][0]
	d.BoxMap[info.Type] = d.BoxMap[info.Type][1:]
	fmt.Fprintln(d.W, "	recv:", m.Body)
	return []Message{m}, func(interface{}) error { return nil }, nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	c := &Client{Driver: &FakeDriver{W: os.Stdout, BoxMap: map[string][]Message{}}}
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
