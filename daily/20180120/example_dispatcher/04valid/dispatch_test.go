package main

import (
	"testing"

	"github.com/stretchr/testify/mock"
)

type mockDispatcher struct {
	mock.Mock
}

func (m *mockDispatcher) dispatchForX(status string) {
	_ = m.Called(status)
}

func TestRunX(t *testing.T) {
	d := &mockDispatcher{}
	d.On("dispatchForX", mock.Anything)
	runX(d)
	d.AssertNumberOfCalls(t, "dispatchForX", 1)
}
