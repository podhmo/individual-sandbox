package domain

import (
	"fmt"
	"m/04register-user/db"
)

type User struct {
	db.User
}

// commands, queries, domain service ...
func RegisterUser(user User, callbacks ...func(user User) error) error {
	if err := db.InsertUser(user.User); err != nil {
		return err
	}
	for _, cb := range callbacks {
		cb := cb
		if err := cb(user); err != nil {
			return err
		}
	}
	return nil
}

func NotifyNewUserRegisteredCallback(notificator Notificator) func(user User) error {
	return func(user User) error {
		title := "#new-users"
		content := fmt.Sprintf(`registered user: {id: %d, name: %q}) .`, user.ID, user.Name)
		return notificator.NotifyMessage(title, content)
	}
}

func SendVerifyMailCallback(mailer Mailer) func(user User) error {
	return func(user User) error {
		title := "need verify"
		content := fmt.Sprintf(`Please [verify your account](https://example.net/users/verify/%d) .`, user.ID)
		return mailer.SendMail(title, content, []string{"foo@example.net"})
	}
}
