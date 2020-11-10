package main

import "fmt"

// https://github.com/google/go-github/blob/76c3c3d7c6e78e8c91e77d2e2578c4e0a7cf96ea/github/github.go#L256
// https://github.com/google/go-github/blob/76c3c3d7c6e78e8c91e77d2e2578c4e0a7cf96ea/github/actions_workflows.go#L105

type Base struct {
	Name   string
	client *Client
}

func (b *Base) Hello() string {
	return fmt.Sprintf("Hello %s", b.Name)
}

type Use Base

func (u *Use) Say() {
	fmt.Println(u.Hello())
}

type Client struct {
	base *Base
	Use  *Use
}

func main() {
	b := &Base{Name: "World"}
	c := &Client{
		base: b,
		Use:  (*Use)(b),
	}
	c.base.client = c
	c.Use.Say()
}
