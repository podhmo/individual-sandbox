package main

import "m/slack"

func main() {
	c := slack.NewClient()
	c.PostMessage("hello")
	c.PostMessage("byebye")
}
