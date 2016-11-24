// Automatically generated by MockGen. DO NOT EDIT!
// Source: src.go

package hmm2

import (
	gomock "github.com/golang/mock/gomock"
)

// Mock of Bower interface
type MockBower struct {
	ctrl     *gomock.Controller
	recorder *_MockBowerRecorder
}

// Recorder for MockBower (not exported)
type _MockBowerRecorder struct {
	mock *MockBower
}

func NewMockBower(ctrl *gomock.Controller) *MockBower {
	mock := &MockBower{ctrl: ctrl}
	mock.recorder = &_MockBowerRecorder{mock}
	return mock
}

func (_m *MockBower) EXPECT() *_MockBowerRecorder {
	return _m.recorder
}

func (_m *MockBower) Bow(s string) S {
	ret := _m.ctrl.Call(_m, "Bow", s)
	ret0, _ := ret[0].(S)
	return ret0
}

func (_mr *_MockBowerRecorder) Bow(arg0 interface{}) *gomock.Call {
	return _mr.mock.ctrl.RecordCall(_mr.mock, "Bow", arg0)
}