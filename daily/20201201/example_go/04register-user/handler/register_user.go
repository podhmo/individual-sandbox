package handler

import (
	"context"
	"encoding/json"
	"io"
	"m/04register-user/domain"
	"m/04register-user/interactor"
)

// controller, handler
func (app *App) RegisterUser(ctx context.Context, r io.Reader) error {
	var user domain.User
	if err := json.NewDecoder(r).Decode(&user); err != nil {
		return err
	}
	mailer := app.GetMailer()
	notificator := app.GetNotificator()

	if err := interactor.RegisterUser(
		ctx,
		mailer,
		notificator,
		user,
	); err != nil {
		return err
	}
	return nil
}
