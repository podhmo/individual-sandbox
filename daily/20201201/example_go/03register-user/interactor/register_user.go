package interactor

import (
	"context"
	"m/03register-user/domain"
)

func RegisterUser(
	ctx context.Context,
	getMailer func() domain.Mailer,
	getNotificator func() domain.Notificator,
	user domain.User,
) error {
	mailer := getMailer()
	notificator := getNotificator()

	if err := domain.RegisterUser(user,
		domain.SendVerifyMailCallback(mailer),
		domain.NotifyNewUserRegisteredCallback(notificator),
	); err != nil {
		return err
	}
	return nil
}
