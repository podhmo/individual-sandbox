package main

import "testing"

type DB struct{}

func GetDB() (DB, error) { return DB{}, nil }

func OrFatal[T any](ob T, err error) func(*testing.T) T {
	return func(t *testing.T) T {
		if err != nil {
			t.Fatalf("hmm: %+v", err)
		}
		return ob
	}
}

func TestIt(t *testing.T) {
	db := OrFatal(GetDB())(t)
	t.Logf("db: %+v", db)
}
