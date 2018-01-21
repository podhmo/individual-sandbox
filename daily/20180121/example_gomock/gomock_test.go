package example

import (
	"testing"

	mock "./gengomock"
	hello "./hello"
	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"
)

func TestGoMock(t *testing.T) {
	t.Run("called", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		defer ctrl.Finish()

		mHello := mock.NewMockHello(ctrl)
		mHello.EXPECT().Hello("hello world").Times(1)
		hello.HelloWorld(mHello, true)
	})

	t.Run("not called", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		defer ctrl.Finish()

		mHello := mock.NewMockHello(ctrl)
		mHello.EXPECT().Hello("hello world").Times(0)

		hello.HelloWorld(mHello, false)
	})
}

func TestGoMock2(t *testing.T) {
	callAddInts := func(h hello.Hello, x, y int) int {
		hello.HelloWorld(h, true)
		hello.HelloWorld(h, false)
		return x + y
	}

	t.Run("called, but it is not main concern", func(t *testing.T) {
		ctrl := gomock.NewController(t)
		// defer ctrl.Finish() // xxx: supress assertion (calling h.Hello() is not main concern)

		mHello := mock.NewMockHello(ctrl)
		mHello.EXPECT().Hello("hello world")

		got := callAddInts(mHello, 1, 2)
		assert.Exactly(t, 3, got)
	})
}
