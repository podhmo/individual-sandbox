package foo_test

import (
	"testing"

	"github.com/golang/mock/gomock"
	. "github.com/podhmo/foo"
	mock "github.com/podhmo/foo/mock_foo"
)

func TestFoo(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

	m := mock.NewMockFoo(ctrl)
	m.EXPECT().Foo().Return(NG).Times(1)

	actual := Use(m)
	expected := "NG"

	if actual != expected {
		t.Errorf("Use() expected: %v, actual: %v", expected, actual)
	}
}
