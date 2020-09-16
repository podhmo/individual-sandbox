package setup

import (
	"m/config"
	"m/store"

	"github.com/go-chi/httplog"
	"github.com/rs/zerolog"
)

type Setup struct {
	config.Config

	logger *zerolog.Logger
	store  *store.Store
}

func (s *Setup) Logger() *zerolog.Logger {
	if s.logger == nil {
		logger := httplog.NewLogger(s.Log.Name, httplog.Options{
			JSON: s.Log.JSON,
		})
		s.logger = &logger
	}
	return s.logger
}

func (s *Setup) Store() *store.Store {
	if s.store == nil {
		s.store = store.New()
	}
	return s.store
}

func (s *Setup) Finalize() {
	s.Logger()
	s.Store()
}
