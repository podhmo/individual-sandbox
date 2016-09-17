package main

import (
	"bytes"
	"fmt"
	"strings"
	"text/template"
)

/*

通知関係をまとめたstructのNotifierがある。
内部ではmailで通知されるかもしれないし。何らかのweb serviceにpostされるかもしれない。
何にせよ、利用者は何も考えずに Notifier.Notify<Name>() を呼び出せば良い。

この時の各実装をテストしたい時の話
*/

type Mailer struct{}
type PingAPI struct{}
type Notifier struct {
	Mailer  *Mailer
	PingAPI *PingAPI
}

type V map[string]interface{}

type User struct {
	Name string
}

// NotifyUserActivated : 登録されたユーザーが有効になった
func (ms *Notifier) NotifyUserActivated(user User) error {
	return ms.Mailer.MailForActivatedUser(user)
}

// NotifyOnUnknownStatus : 何かよくわかんないけれどおかしい
func (ms *Notifier) NotifyOnUnknownStatus(description string) error {
	return ms.PingAPI.PingUnknownStatus(description)
}

// ping api
func (p *PingAPI) PingUnknownStatus(description string) error {
	return p.Ping("unknown", description)
}

func (p *PingAPI) Ping(code, desc string) error {
	panic(fmt.Sprintf("don't call this: (code=%q)", code))
}

// mailer
func (m *Mailer) MailForActivatedUser(user User) error {
	return m.sendMailTemplateOutput(
		"仮登録 -> 本登録", "templates/user-activate.tmpl", V{"user": user},
	)
}

func (m *Mailer) sendMailTemplateOutput(subject, fileName string, vars V) error {
	var b bytes.Buffer
	tpl := template.Must(template.New("template").ParseFiles(fileName))
	if err := tpl.Execute(&b, vars); err != nil {
		return err
	}
	rendered := b.String()
	body := strings.Trim(rendered, " \r\n\t")
	return m.sendMail(subject, body)
}

func (m *Mailer) sendMail(subject, body string) error {
	panic(fmt.Sprintf("don't call this: (subject=%q)", subject))
}
