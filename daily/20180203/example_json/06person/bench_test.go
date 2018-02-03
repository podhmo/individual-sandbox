package main

import (
	"encoding/json"
	"testing"
)

// BenchWritePerson :
func BenchmarkWritePerson(b *testing.B) {
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
		b := fakeBuffer{}
		WritePerson(&p, &b)
	}
}

// BenchJsonEncoder :
func BenchmarkJsonEncoder(b *testing.B) {
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
		b := fakeBuffer{}
		encoder := json.NewEncoder(&b)
		encoder.Encode(&p)
	}
}

type fakeBuffer struct{}

func (f *fakeBuffer) WriteString(s string) (n int, err error) {
	return len(s), nil
}
func (f *fakeBuffer) Write(b []byte) (n int, err error) {
	return len(b), nil
}
