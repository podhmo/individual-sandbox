package main

import (
	"m/mail"
	"m/separated"
	"m/separated/xxx"
	"m/separated/yyy"
	"m/slack"
)

type dispatcher struct {
	*xxx.XXXDispatcher
	*yyy.YYYDispatcher
}

func main() {
	d := &dispatcher{
		XXXDispatcher: xxx.NewDispatcher(slack.NewClient()),
		YYYDispatcher: yyy.NewDispatcher(mail.NewClient()),
	}
	separated.Use(d)
}
