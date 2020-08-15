package main

import (
	"bytes"
	"encoding/json"
	"testing"
)

func BenchmarkRunInner(b *testing.B) {
	var buf bytes.Buffer
	p := &Person{
		Name: "foo",
		Age:  20,
		Father: &Person{
			Name: "bar",
			Age:  40,
		},
	}
	encoder := json.NewEncoder(&buf)
	encoder.SetIndent("", "  ")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		runInner(p, encoder)
	}
}
