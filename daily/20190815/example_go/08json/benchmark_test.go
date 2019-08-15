package main

import (
	"encoding/json"
	"testing"
)

// Person2 :
type Person2 struct {
	Name   string // required
	Age    int
	Father *Person2
	Mother *Person2
}

func BenchmarkNormalOK(b *testing.B) {
	var p Person2
	body := []byte(`{"name": "foo", "age": 20, "father": {"name": "foo", "age": 40}}`)
	b.ResetTimer()
	if err := json.Unmarshal(body, &p); err != nil {
		panic(err) // never reached
	}
}

func BenchmarkWrapOK(b *testing.B) {
	var p Person
	body := []byte(`{"name": "foo", "age": 20, "father": {"name": "foo", "age": 40}}`)
	b.ResetTimer()
	if err := json.Unmarshal(body, &p); err != nil {
		panic(err) // never reached
	}
}

func BenchmarkNormalNG(b *testing.B) {
	var p Person2
	body := []byte(`{"name": "foo", "age": 20, "father": {"age": 40}, "mother": {"age": 40}}`)
	b.ResetTimer()
	json.Unmarshal(body, &p)
}

func BenchmarkWrapNG(b *testing.B) {
	var p Person
	body := []byte(`{"name": "foo", "age": 20, "father": {"age": 40}, "mother": {"age": 40}}`)
	b.ResetTimer()
	json.Unmarshal(body, &p)
}
