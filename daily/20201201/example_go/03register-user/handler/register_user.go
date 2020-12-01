package handler

import (
	"context"
	"encoding/json"
	"io"
	"m/03register-user/domain"
	"m/03register-user/interactor"
)

// controller, handler
func (app *App) RegisterUser(ctx context.Context, r io.Reader) error {
	var user domain.User
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
