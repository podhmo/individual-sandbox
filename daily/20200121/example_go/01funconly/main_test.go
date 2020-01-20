package main

import (
	"reflect"
	"testing"
)

// FuncNews ...
type FuncNews struct {
	Get func() string
}

type News = *FuncNews

// Use ...
func Use(n News) []string {
	return []string{n.Get(), n.Get(), n.Get()}
}

func TestIt(t *testing.T) {
	news := &FuncNews{Get: func() string { return "hello" }}

	got := Use(news)

	want := []string{"hello", "hello", "hello"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("want %+v, but got %+v", want, got)
	}
}
