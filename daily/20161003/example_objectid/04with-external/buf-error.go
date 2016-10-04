package main

import (
	"bytes"
	"fmt"
)

type bufError struct {
	buf bytes.Buffer
}

func (be *bufError) WriteString(s string) {
	be.buf.WriteString(s)
}
func (be *bufError) Error() error {
	msg := be.buf.String()
	if len(msg) > 0 {
		return fmt.Errorf(msg)
	}
	return nil
}
func newBufError() *bufError {
	var buf bytes.Buffer
	be := bufError{buf: buf}
	return &be
}
