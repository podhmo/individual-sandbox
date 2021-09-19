package main

import (
	"fmt"
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
	P interface{ Println(...interface{}) }
}

func (c *FakeConnector) Connect(x string) error {
	c.P.Println(x)
	return nil
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

type Provider struct {
	*SlackConfig
}

func (p *Provider) NewSlackClient() *SlackClient {
	// strconv.ParseBool(os.Getenv("MOCK"))
	connector := &FakeConnector{P: log.New(os.Stdout, "fake slack: ", 0)}
	return p.SlackConfig.NewSlackClient(connector)
}

func main() {
	config := &Config{}
	config.Slack.DefaultChanel = "random"

	p := &Provider{SlackConfig: &config.Slack}
	if err := run(p); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(p interface {
	NewSlackClient() *SlackClient
}) error {
	client := p.NewSlackClient()
	return Hello(client)
}

func Hello(client *SlackClient) error {
	s := client.PostMessage()
	s.User = "me"
	return s.Do("hello world")
}
