package web

import (
	"errors"
	"net/http"
	"runtime/debug"

	"github.com/go-chi/chi/middleware"
)

type APIError interface {
	error
	StatusCode() int
}

type CustomHandler func(http.ResponseWriter, *http.Request) error

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

					s.SendObjectWithStatus(w, r, map[string]interface{}{
						"panic":   true,
						"code":    500,
						"message": rvr,
					}, 500)
				}
			}()

			err := handle(w, r)
			if err == nil {
				return
			}

			var rawErr APIError
			statusCode := 500
			message := err.Error()
			if errors.As(err, &rawErr) {
				statusCode = rawErr.StatusCode()
				message = rawErr.Error() // omit unwrapped message
			}

			// TODO: verbose logging on DEBUG=true

			// need logging?
			s.SendObjectWithStatus(w, r, map[string]interface{}{
				"code":    statusCode,
				"message": message,
			}, statusCode)
		}
	}
}
