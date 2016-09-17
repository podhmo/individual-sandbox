package main

import (
	"fmt"
)

type mailerClient interface {
	sendMail(subject, body string) error
}

type ActualmailerClient struct {
}

func (c *ActualmailerClient) sendMail(subject, body string) error {
	// テストの時などここの処理は呼ばれてほしくない。
	panic(fmt.Sprintf("don't call this (subject=%q)", subject))
}

type Mailer struct {
	mailerClient
}

func NewMailer() *Mailer {
	client := &ActualmailerClient{}
	mailer := &Mailer{mailerClient: client}
	return mailer
}

func (m *Mailer) SendXMail() error {
	subject := "about x"
	body := fmt.Sprintf("body: %q", "xxx")
	return m.sendMail(subject, body)
}

type mockedClient struct {
}

func (m *mockedClient) sendMail(subject, body string) error {
	fmt.Println(subject)
	fmt.Println("----------------------------------------")
	fmt.Println(body)
	return nil
}

func main() {
	{
		client := &mockedClient{}
		m := Mailer{mailerClient: client}
		m.SendXMail()
	}
}
