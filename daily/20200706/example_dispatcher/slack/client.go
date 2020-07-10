package slack

import (
	"fmt"
	"io"
	"os"
)

type Client struct {
	defaultChannel string
	writer         io.Writer
}

func (c *Client) PostMessage(message string) error {
	fmt.Fprintf(c.writer, "\tslack\t%s\t%s\n", c.defaultChannel, message)
	return nil
}

func NewClient() *Client {
	return &Client{
		defaultChannel: "#random",
		writer:         os.Stdout,
	}
}
