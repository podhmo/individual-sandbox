package example

import (
	"testing"

	genmock "./genmock"
	hello "./hello"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestTestifyMock(t *testing.T) {
	t.Run("called", func(t *testing.T) {
		mHello := &genmock.Hello{}
		mHello.On("Hello", "hello world")

		hello.HelloWorld(mHello, true)

		mHello.AssertNumberOfCalls(t, "Hello", 1)
	})

	t.Run("not called", func(t *testing.T) {
		mHello := &genmock.Hello{}
		mHello.On("Hello", "hello world")

		hello.HelloWorld(mHello, false)

		mHello.AssertNumberOfCalls(t, "Hello", 0)
	})
}

func TestTestifyMock2(t *testing.T) {
	callAddInts := func(h hello.Hello, x, y int) int {
		hello.HelloWorld(h, true)
		hello.HelloWorld(h, false)
		return x + y
	}

	t.Run("called, but it is not main concern", func(t *testing.T) {
		mHello := &genmock.Hello{}
		mHello.On("Hello", mock.Anything)

		got := callAddInts(mHello, 1, 2)
		assert.Exactly(t, 3, got)
	})
}
