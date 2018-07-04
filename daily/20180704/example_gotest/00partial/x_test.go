package x

import "testing"

type FakeXF struct {
	X
	f func() string
}

func (x *FakeXF) F() string {
	return x.f()
}

func TestFforX(t *testing.T) {
	x := &FakeXF{f: func() string { return "f" }}
	got := x.F()
	if got != "f" {
		t.Errorf("expected f but got %v", got)
	}
}
