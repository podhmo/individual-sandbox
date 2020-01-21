// +build mock

package main

import (
	"fmt"
	"strings"
	"testing"
)

// News ...
type News struct {
	S string
}

// Get ...
func (n *News) Get() string {
	return fmt.Sprintf("%s!!", strings.Title(n.S))
}

func TestIt(t *testing.T) {
	news := &News{S: "hello"}

	got := news.Get()
	want := "Hello!!"

	if got != want {
		t.Errorf("want %q, but %q", want, got)
	}
}
