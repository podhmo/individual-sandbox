package main

import (
	"io"

	"github.com/k0kubun/pp"
)

// Client :
type Client struct {
	Path string
	Body io.Reader
}

// NewGetClient :
func NewGetClient(options ...GetOption) *Client {
	c := &Client{}
	for _, opt := range options {
		opt.ApplyForGet(c)
	}
	return c
}

// NewPostClient :
func NewPostClient(options ...PostOption) *Client {
	c := &Client{}
	for _, opt := range options {
		opt.ApplyForPost(c)
	}
	return c
}

// GetOption :
type GetOption interface{ ApplyForGet(*Client) }

// PostOption :
type PostOption interface{ ApplyForPost(*Client) }

// WithPath :
func WithPath(path string) interface {
	GetOption
	PostOption
} {
	return withPath{path}
}

type withPath struct{ path string }

func (w withPath) ApplyForPost(c *Client) {
	c.Path = w.path
}
func (w withPath) ApplyForGet(c *Client) {
	c.Path = w.path
}

// WithBody :
func WithBody(body io.Reader) interface {
	PostOption
} {
	return withBody{body}
}

type withBody struct{ body io.Reader }

func (w withBody) ApplyForPost(c *Client) {
	c.Body = w.body
}

func main() {
	pp.Println(NewPostClient(
		WithPath("xxx"),
		WithBody(nil),
	))
	pp.Println(NewPostClient(
		WithPath("xxx"),
	))
}
