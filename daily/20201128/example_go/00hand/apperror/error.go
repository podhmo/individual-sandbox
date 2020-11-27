package apperror

func New(err error, code int) error {
	return &appError{err: err, code: code}
}

type appError struct {
	err  error
	code int
}

func (e *appError) Error() string {
	return e.err.Error()
}

func (e *appError) Code() int {
	return e.code
}
