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

type Driver interface {
	SendMessage(ctx context.Context, m Message) error
	ReciveMessage(ctx context.Context, info QueueInfo) (<-chan Message, error)
}

type Client struct {
	Driver Driver
}

type PingMessage struct {
	Text      string    `json:"text"`
	CreatedAt time.Time `json:"createdAt"`
}

func (c *Client) SendPing(ctx context.Context, m PingMessage) error {
	b, err := json.Marshal(m)
	if err != nil {
		return xerrors.Errorf("json encode: %w", err)
	}
	raw := Message{Body: string(b), QueueInfo: QueueInfo{Type: "ping"}}
	return c.Driver.SendMessage(ctx, raw)
}

func (c *Client) RecivePing(ctx context.Context) (<-chan PingMessage, error) {
	ch := make(chan PingMessage, 1)
	defer close(ch)

	rawCh, err := c.Driver.ReciveMessage(ctx, QueueInfo{Type: "ping"})
	if err != nil {
		return ch, xerrors.Errorf("recv message: %w", err)
	}
	for raw := range rawCh {
		var m PingMessage
		if err := json.Unmarshal([]byte(raw.Body), &m); err != nil {
			return ch, xerrors.Errorf("json decode: %w", err)
		}
		ch <- m
	}
	return ch, nil
}

type FakeDriver struct {
	W      io.Writer
	BoxMap map[string][]Message
}

func (d *FakeDriver) SendMessage(ctx context.Context, m Message) error {
	fmt.Fprintln(d.W, "	send:", m.Body)
	d.BoxMap[m.Type] = append(d.BoxMap[m.Type], m)
	return nil
}
func (d *FakeDriver) ReciveMessage(ctx context.Context, info QueueInfo) (<-chan Message, error) {
	ch := make(chan Message, 1)
	defer close(ch)

	m := d.BoxMap[info.Type][0]
	d.BoxMap[info.Type] = d.BoxMap[info.Type][1:]
	fmt.Fprintln(d.W, "	recv:", m.Body)
	ch <- m
	return ch, nil
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
		ch, err := c.RecivePing(ctx)
		if err != nil {
			return xerrors.Errorf("recv ping: %w", err)
		}
		for m := range ch {
			fmt.Println("got", m)
		}
	}
	return nil
}
