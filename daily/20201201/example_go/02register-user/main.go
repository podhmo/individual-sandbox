package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
)

type User struct {
	ID   int    `json:"id"`
	Name string `json:"Name"`
}

// domain
var users []User

// commands, queries, domain service ...
func RegisterUser(user User, callbacks ...func(user User) error) error {
	users = append(users, user)
	for _, cb := range callbacks {
		cb := cb
		if err := cb(user); err != nil {
			return err
		}
	}
	return nil
}

type Mailer interface {
	SendMail(title string, content string, dests []string) error
}
type FakeMailer struct {
	w io.Writer
}

func (m *FakeMailer) SendMail(title string, content string, dests []string) error {
	fmt.Fprintln(m.w, "send mail")
	fmt.Fprintf(m.w, "\ttitle: %s\n", title)
	for _, dest := range dests {
		fmt.Fprintf(m.w, "\t\tto: %s\n", dest)
	}
	fmt.Fprintln(m.w, "\t----------------------------------------")
	for _, line := range strings.Split(content, "\n") {
		fmt.Fprintln(m.w, "\t", line)
	}
	return nil
}

func SendVerifyMailCallback(mailer Mailer) func(user User) error {
	return func(user User) error {
		title := "need verify"
		content := fmt.Sprintf(`Please [verify your account](https://example.net/users/verify/%d) .`, user.ID)
		return mailer.SendMail(title, content, []string{"foo@example.net"})
	}
}

type Notificator interface {
	NotifyMessage(channel string, content string) error
}

type FakeNotificator struct {
	w io.Writer
}

func (m *FakeNotificator) NotifyMessage(channel string, content string) error {
	fmt.Fprintln(m.w, "notify message")
	fmt.Fprintln(m.w, "\t- - - - - - - - - - - - - - - - - - - - ")
	fmt.Fprintf(m.w, "\tchannel: %s\n", channel)
	fmt.Fprintln(m.w, "\t- - - - - - - - - - - - - - - - - - - - ")
	fmt.Fprintln(m.w, content)
	return nil
}

func NotifyNewUserRegisteredCallback(notificator Notificator) func(user User) error {
	return func(user User) error {
		title := "new user is registered"
		content := fmt.Sprintf(`registered user: {id: %d, name: %q}) .`, user.ID, user.Name)
		return notificator.NotifyMessage(title, content)
	}
}

// interactor, usecase
type Interactor struct {
}

func (interactor *Interactor) RegisterUser(
	ctx context.Context,
	getMailer func() Mailer,
	getNotificator func() Notificator,
	user User,
) error {
	mailer := getMailer()
	notificator := getNotificator()

	if err := RegisterUser(user,
		SendVerifyMailCallback(mailer),
		NotifyNewUserRegisteredCallback(notificator),
	); err != nil {
		return err
	}
	return nil
}

type Registry struct {
	GetMailer      func() Mailer
	GetNotificator func() Notificator
}

type App struct {
	Registry
}

// controller, handler
func (app *App) RegisterUser(ctx context.Context, r io.Reader) error {
	interactor := &Interactor{}
	var user User
	if err := json.NewDecoder(r).Decode(&user); err != nil {
		return err
	}
	if err := interactor.RegisterUser(
		ctx,
		app.GetMailer,
		app.GetNotificator,
		user,
	); err != nil {
		return err
	}
	return nil
}

func main() {
	run := func() error {
		app := &App{}

		mailer := &FakeMailer{w: os.Stdout}
		app.GetMailer = func() Mailer { return mailer }

		notificator := &FakeNotificator{w: os.Stdout}
		app.GetNotificator = func() Notificator { return notificator }

		return Run(app)
	}

	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Run(app *App) error {
	ctx := context.Background()
	return app.RegisterUser(ctx, bytes.NewBufferString(`{"id": 1, "name": "foo"}`))
}
