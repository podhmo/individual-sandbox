package hmm2

import (
	"testing"

	"github.com/golang/mock/gomock"
)

func TestBow(t *testing.T) {
	ctrl := gomock.NewController(t)
	m := NewMockBower(ctrl)
	defer ctrl.Finish()

	m.EXPECT().Bow("foo").Return(S("20")).Times(1)
	actual := f(m, "foo")
	expected := S("20")
	if actual != expected {
		t.Errorf("actual %s, expected: %s", actual, expected)
	}
    t.Log("yay")
}
