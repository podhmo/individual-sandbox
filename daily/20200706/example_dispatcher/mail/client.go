package mail

import (
	"fmt"
	"io"
	"os"
)

type Client struct {
	sender string
	writer io.Writer
}

func (c *Client) SendMail(message string) error {
	fmt.Fprintf(c.writer, "\tmail\t%s\t%s\n", c.sender, message)
	return nil
}

func NewClient() *Client {
	return &Client{
		sender: "example@dot.net",
		writer: os.Stdout,
	}
}
