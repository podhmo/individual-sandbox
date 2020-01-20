package main

import (
	"reflect"
	"testing"
)

type News interface {
	Get() string
}

// FuncNews ...
type FuncNews struct {
	get func() string
}

// Get ...
func (n *FuncNews) Get() string {
	return n.get()
}

// Use ...
func Use(n News) []string {
	return []string{n.Get(), n.Get(), n.Get()}
}

func TestIt(t *testing.T) {
	news := &FuncNews{get: func() string { return "hello" }}

	got := Use(news)

	want := []string{"hello", "hello", "hello"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("want %+v, but got %+v", want, got)
	}
}
