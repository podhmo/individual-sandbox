package interactor

import (
	"context"
	"m/04register-user/domain"
)

func RegisterUser(
	ctx context.Context,
	mailer domain.Mailer,
	notificator domain.Notificator,
	user domain.User,
) error {
	if err := domain.RegisterUser(user,
		domain.SendVerifyMailCallback(mailer),
		domain.NotifyNewUserRegisteredCallback(notificator),
	); err != nil {
		return err
	}
	return nil
}
