package main

import (
	"fmt"
)

type Mailer struct {
}

func (m *Mailer) SendXMail() error {
	subject := "about x"
	body := fmt.Sprintf("body: %q", "xxx")
	return m.sendMail(subject, body)
}

func (m *Mailer) sendMail(subject, body string) error {
	// テストの時などここの処理は呼ばれてほしくない。
	panic(fmt.Sprintf("don't call this (subject=%q)", subject))
}

// 実際のMailer.sendMailは呼びたくない。ただしSendXMailの処理は実行させたい。
// もちろん、以下の様に書くとダメ。

type MockedMailer struct {
	*Mailer
}

func (m *MockedMailer) sendMail(subject, body string) error {
	fmt.Println(subject)
	fmt.Println("----------------------------------------")
	fmt.Println(body)
	return nil
}

func main() {
	m := Mailer{}
	{
		m := MockedMailer{Mailer: &m}
		m.SendXMail()
	}
}
