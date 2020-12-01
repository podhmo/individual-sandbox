package handler

import "m/03register-user/domain"

type Registry struct {
	GetMailer      func() domain.Mailer
	GetNotificator func() domain.Notificator
}

type App struct {
	Registry
}
