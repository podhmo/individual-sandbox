package interactor

import (
	"bytes"
	"context"
	"m/03register-user/component/mailer"
	"m/03register-user/component/notificator"
	"m/03register-user/db"
	"m/03register-user/domain"
	"strings"
	"testing"
)

func TestRegisterUser(t *testing.T) {
	db.Clear()

	var user domain.User
	user.ID = 1
	user.Name = "foo"
	ctx := context.Background()

	var mailerBuf bytes.Buffer
	var notificatorBuf bytes.Buffer

	err := RegisterUser(
		ctx,
		func() domain.Mailer { return &mailer.FakeMailer{W: &mailerBuf} },
		func() domain.Notificator { return &notificator.FakeNotificator{W: &notificatorBuf} },
		user,
	)

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	t.Run("db", func(t *testing.T) {
		if want, got := 1, len(db.GetUsers()); want != got {
			t.Errorf("want\n\t%v\nbut got\n\t%v",
				want, got,
			)
		}
	})

	t.Run("send mail", func(t *testing.T) {
		want := `send mail
	title: need verify
		to: foo@example.net
	----------------------------------------
	 Please [verify your account](https://example.net/users/verify/1) .
notify message
`
		got := mailerBuf.String()
		if want != got {
			t.Errorf("want\n\t%s\nbut got\n\t%s",
				strings.TrimSpace(want), strings.TrimSpace(got),
			)
		}
	})

	t.Run("notify message", func(t *testing.T) {
		want := `notify message
	- - - - - - - - - - - - - - - - - - - - 
	channel: new user is registered
	- - - - - - - - - - - - - - - - - - - - 
	 registered user: {id: 1, name: "foo"}) .
`
		got := notificatorBuf.String()
		if want != got {
			t.Errorf("want\n\t%s\nbut got\n\t%s",
				strings.TrimSpace(want), strings.TrimSpace(got),
			)
		}
	})
}
