package web

import (
	"errors"
	"fmt"
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

			var apiErr APIError
			var innerErr error = err
			statusCode := 500

			if errors.As(err, &apiErr) {
				statusCode = apiErr.StatusCode()
				innerErr = apiErr
			}
			for inner, ok := innerErr.(interface {
				Unwrap() error
			}); ok; {
				next := inner.Unwrap()
				if innerErr == next {
					break
				}
				innerErr = next
			}

			// TODO: verbose logging on DEBUG=true
			if statusCode == 500 {
				// warn? error ?
				s.Logger.Error().Str("verbose-error", fmt.Sprintf("%#+v", err)).Msgf("ERROR")
			}

			// need logging?
			s.SendObjectWithStatus(w, r, map[string]interface{}{
				"code":    statusCode,
				"message": innerErr.Error(), // omit unwrapped message,
			}, statusCode)
		}
	}
}
