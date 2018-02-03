package main

import (
	"bytes"
	"testing"
)

// BenchSerialize :
func BenchmarkSerialize(b *testing.B) {
	p := Person{
		ID:       1,
		Name:     "akane",
		Birthday: "08-16",
		VividInfo: VividInfo{
			Color:  "red",
			Weapon: "Rang",
		},
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		var b bytes.Buffer
		WritePerson(&p, &b)
	}
}
