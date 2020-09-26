package slacktest

import (
	"context"
	"log"
	"m/06separated/slackdriver"
	"strings"
	"testing"
)

type HasDriver interface {
	Driver() slackdriver.Driver
}
type Logger interface {
	Printf(fmt string, args ...interface{})
}
type Message struct {
	Channel string
	Text    string
}
type Box []Message
type FakeDriver struct {
	Logger Logger
	Box    Box
}

func NewDriver(options ...func(*FakeDriver)) *FakeDriver {
	d := &FakeDriver{}
	for _, opt := range options {
		opt(d)
	}
	return d
}
func WithLogger(l *log.Logger) func(*FakeDriver) {
	return func(d *FakeDriver) {
		d.Logger = l
	}
}
func (d *FakeDriver) SendMessage(ctx context.Context, channel, text string) error {
	m := Message{
		Channel: channel,
		Text:    text,
	}
	if d.Logger != nil {
		log.Printf("send message (%s): %s", m.Channel, strings.TrimRight(strings.ReplaceAll(m.Text, "\n", "\n\t"), "\n\t"))
	}
	d.Box = append(d.Box, m)
	return nil
}

func AssertWithBox(t *testing.T, client HasDriver, assert func(t *testing.T, box Box)) {
	t.Helper()
	driver, ok := client.Driver().(*FakeDriver)
	if !ok {
		t.Fatalf("invalid type: %T", client.Driver)
	}

	box := driver.Box
	t.Logf("box length %v", len(box))
	if len(box) == 0 {
		t.Fatalf("expect not empty")
	}

	assert(t, box)
}
