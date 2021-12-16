package code

import (
	"github.com/morikuni/failure"
)

// error codes for your application.
const (
	NotFound        failure.StringCode = "NotFound"
	Unauthorized    failure.StringCode = "Unauthorized"
	Forbidden       failure.StringCode = "Forbidden"
	ValidationError failure.StringCode = "ValidationError"
)

func HTTPStatusOf(err error) int {
	if err == nil {
		return 200 // http.StatusOK
	}

	c, ok := failure.CodeOf(err)
	if !ok {
		return 500 // http.StatusInternalServerError
	}
	switch c {
	case Unauthorized:
		return 401 // http.StatusUuauthorized
	case Forbidden:
		return 403 // http.StatusForbidden
	case NotFound:
		return 404 // http.StatusNotFound
	case ValidationError:
		return 422 // http.StatusUnprocessableEntity // or http.StatusBadRequest?
	default:
		return 500 // http.StatusInternalServerError
	}
}
