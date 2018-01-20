package main

import (
	"testing"

	"github.com/stretchr/testify/mock"
)

func TestApp(t *testing.T) {
	fakeDispatcher := func() dispatcher {
		md := &mockDispatcher{}
		md.On("dispatchForX", mock.Anything).Return(nil)
		return md
	}

	t.Run("通知が呼ばれる場合", func(t *testing.T) {
		app := &app{dispatcher: fakeDispatcher()}
		call := true
		app.run(call)
	})
	t.Run("通知が呼ばれない場合", func(t *testing.T) {
		app := &app{dispatcher: fakeDispatcher()}
		call := false
		app.run(call)
	})
}
