package domain

type Mailer interface {
	SendMail(title string, content string, dests []string) error
}

type Notificator interface {
	NotifyMessage(channel string, content string) error
}
