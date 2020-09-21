package web

import (
	"net/http"

	"github.com/go-chi/render"
)

func (s *Server) SendObject(w http.ResponseWriter, r *http.Request, ob interface{}) error {
	render.JSON(w, r, ob)
	return nil
}

func (s *Server) SendObjectWithStatus(w http.ResponseWriter, r *http.Request, ob interface{}, statusCode int) error {
	render.Status(r, statusCode)
	return s.SendObject(w, r, ob)
}

func (s *Server) SendArray(w http.ResponseWriter, r *http.Request, items interface{}, size int) error {
	// TODO: more good interface
	if size == 0 {
		items = []bool{} // zero length array
	}
	// TODO: https://opensource.zalando.com/restful-api-guidelines
	render.JSON(w, r, map[string]interface{}{"items": items})
	return nil
}
func (s *Server) SendArrayWithStatus(w http.ResponseWriter, r *http.Request, items interface{}, statusCode int, size int) error {
	// TODO: https://opensource.zalando.com/restful-api-guidelines
	render.Status(r, statusCode)
	return s.SendArray(w, r, items, size)
}
