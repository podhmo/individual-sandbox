package main

import "fmt"

type service struct {
	client *Client
}

type FooService service

func (s *FooService) Do() {
	s.client.Do("Foo")
}

type BarService service

func (s *BarService) Do() {
	s.client.Do("Bar")
}

type Client struct {
	service *service

	Foo *FooService
	Bar *BarService
}

func (c *Client) Do(message string) {
	fmt.Println("Do", message)
}

func NewClient() *Client {
	gw := &service{}
	client := &Client{
		Foo: (*FooService)(gw),
		Bar: (*BarService)(gw),
	}
	gw.client = client
	return client
}

func main() {
	client := NewClient()
	client.Foo.Do()
	client.Bar.Do()
}
