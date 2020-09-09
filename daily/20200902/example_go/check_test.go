package main

import (
	"fmt"
	"testing"

	"github.com/pkg/errors"
)

func Test00(t *testing.T) {
	// b.f, b.g, b.h
	want := "! on f: on g: xxx"

	var f func() error
	var g func() error
	var h func() error
	f = func() error {
		return errors.WithMessage(g(), "on f")
	}
	g = func() error {
		return errors.WithMessage(h(), "on g")
	}
	h = func() error {
		return errors.New("xxx")
	}
	err := f()

	t.Logf("!! %+v", err)
	got := fmt.Sprintf("! %v", err)

	if got != want {
		t.Errorf("want %q, but got %q", want, got)
	}
}
