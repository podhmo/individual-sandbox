package web

import (
	"fmt"
	"m/store"
	"net/http"
	"runtime/debug"

	"github.com/go-chi/chi/middleware"
)

type APIError interface {
	error
	StatusCode() int
}

type AppSession interface {
	store.StoreFactory
}

type CustomHandler func(
	AppSession,
	http.ResponseWriter,
	*http.Request,
) error

func NewDefaultHandler(s *Server) func(CustomHandler) http.HandlerFunc {
	// TODO: stop closure
	return func(handle CustomHandler) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			defer func() {
				if rvr := recover(); rvr != nil {
					logEntry := middleware.GetLogEntry(r)
					if logEntry != nil {
						logEntry.Panic(rvr, debug.Stack())
					} else {
						middleware.PrintPrettyStack(rvr)
					}

					s.SendObject(w, r, map[string]interface{}{
						"panic":   true,
						"code":    500,
						"message": rvr,
					}, s.WithStatusCode(500))
				}
			}()

			as := s.setup.NewAppSession()
			err := handle(as, w, r)
			if closeErr := as.Close(); err != nil {
				s.Logger.Error().Str("verbose-error", fmt.Sprintf("%#+v", closeErr)).Msgf("ERROR")

			}

			if err == nil {
				return
			}
			if err := s.SendError(w, r, err); err != nil {
				panic(err)
			}
		}
	}
}
