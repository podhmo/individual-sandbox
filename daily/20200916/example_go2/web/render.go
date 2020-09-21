package web

import (
	"errors"
	"net/http"

	"github.com/go-chi/render"
)

type SendParams struct {
	StatusCode int
}
type SendOption func(*SendParams)

func (s *Server) WithStatusCode(n int) SendOption {
	return func(p *SendParams) {
		p.StatusCode = n
	}
}

func (s *Server) SendObject(
	w http.ResponseWriter,
	r *http.Request,
	ob interface{},
	options ...func(*SendParams),
) error {
	params := &SendParams{
		StatusCode: 200,
	}
	for _, opt := range options {
		opt(params)
	}
	render.Status(r, params.StatusCode)
	render.JSON(w, r, ob)
	return nil
}

func (s *Server) SendArray(
	w http.ResponseWriter,
	r *http.Request,
	items interface{},
	options ...func(*SendParams),
) error {
	// TODO: more good interface
	if items == nil {
		items = []bool{} // zero length array
	}

	params := &SendParams{
		StatusCode: 200,
	}
	for _, opt := range options {
		opt(params)
	}
	render.Status(r, params.StatusCode)
	// TODO: https://opensource.zalando.com/restful-api-guidelines
	render.JSON(w, r, map[string]interface{}{"items": items})
	return nil
}

func (s *Server) SendError(
	w http.ResponseWriter,
	r *http.Request,
	err error,
	options ...func(*SendParams),
) error {
	if err == nil {
		return nil
	}

	params := &SendParams{
		StatusCode: 500,
	}
	for _, opt := range options {
		opt(params)
	}
	var apiErr APIError
	var innerErr error = err

	if errors.As(err, &apiErr) {
		params.StatusCode = apiErr.StatusCode()
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
	if params.StatusCode == 500 {
		// warn? error ?
		s.Logger.Error().Str("short", err.Error()).Msgf("%+v", err)
	}

	// need logging?
	render.Status(r, params.StatusCode)
	render.JSON(w, r, map[string]interface{}{
		"code":    params.StatusCode,
		"message": innerErr.Error(), // omit unwrapped message,
	})
	return nil
}
