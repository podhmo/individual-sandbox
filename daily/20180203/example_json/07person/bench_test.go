package main

import (
	"bytes"
	"encoding/json"
	"testing"

	"./internal"
)

// BenchSerialize :
func BenchmarkSerialize(b *testing.B) {
	p := internal.Person{
		ID:       1,
		Name:     "akane",
		Birthday: "08-16",
		VividInfo: internal.VividInfo{
			Color:  "red",
			Weapon: "Rang",
		},
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		var b bytes.Buffer
		encoder := json.NewEncoder(&b)
		encoder.Encode(&p)
	}
}
