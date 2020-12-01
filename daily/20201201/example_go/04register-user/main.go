package main

import (
	"bytes"
	"context"
	"log"
	"m/04register-user/component/mailer"
	"m/04register-user/component/notificator"
	"m/04register-user/domain"
	"m/04register-user/handler"
	"os"
)

func main() {
	run := func() error {
		app := &handler.App{}

		mailer := &mailer.FakeMailer{W: os.Stdout}
		app.GetMailer = func() domain.Mailer { return mailer }

		notificator := &notificator.FakeNotificator{W: os.Stdout}
		app.GetNotificator = func() domain.Notificator { return notificator }

		return Run(app)
	}

	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Run(app *handler.App) error {
	ctx := context.Background()
	return app.RegisterUser(ctx, bytes.NewBufferString(`{"id": 1, "name": "foo"}`))
}
