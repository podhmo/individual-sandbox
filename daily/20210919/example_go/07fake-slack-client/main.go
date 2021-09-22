package main

import (
	"fmt"
	"io"
	"log"
	"os"
)

type Config struct {
	Slack SlackConfig
}

type SlackConfig struct {
	Token         string
	DefaultChanel string
}

func (c *SlackConfig) NewSlackClient(connector Connector) *SlackClient {
	return &SlackClient{Config: c, Connector: connector}
}

type SlackClient struct {
	Config    *SlackConfig
	Connector Connector
}

type Connector interface {
	Connect(string) error
}
type FakeConnector struct {
	W io.Writer
}

func (c *FakeConnector) Connect(x string) error {
	_, err := fmt.Fprintln(c.W, x)
	return err
}

ｍtype StdoutConnectorFactory struct{}

func (f *StdoutConnectorFactory) NewConnector() Connector {
	return &FakeConnector{W: os.Stdout}
}

func (c *SlackClient) PostMessage() *PostMessage {
	return &PostMessage{client: c, User: "somesone", Channel: c.Config.DefaultChanel}
}

// https://api.slack.com/methods/chat.postMessage
type PostMessage struct {
	User    string
	Channel string
	client  *SlackClient
}

func (s *PostMessage) Do(text string) error {
	return s.client.Connector.Connect(fmt.Sprintf("in #%s\n\t%s: %s\n", s.Channel, s.User, text))
}

func main() {
	config := &Config{}
	config.Slack.DefaultChanel = "random"

	p := struct {
		*SlackConfig
		*StdoutConnectorFactory
	}{
		SlackConfig:            &config.Slack,
		StdoutConnectorFactory: &StdoutConnectorFactory{},
	}
	if err := run(p); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(p interface {
	NewConnector() Connector
	NewSlackClient(Connector) *SlackClient
}) error {
	connector := p.NewConnector()
	client := p.NewSlackClient(connector)
	return Hello(client)
}

func Hello(client *SlackClient) error {
	s := client.PostMessage()
	s.User = "me"
	return s.Do("hello world")
}
