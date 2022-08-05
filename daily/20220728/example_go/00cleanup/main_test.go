package main

import (
	"testing"
)

type DB struct{}

func (db *DB) Close() error {
	return nil
}

type Option func(*DB) error

func NewDB(options ...Option) (*DB, func() error, error) {
	db := &DB{}
	for _, opt := range options {
		if err := opt(db); err != nil {
			return nil, func() error { return nil }, err
		}
	}
	return db, func() error { return db.Close() }, nil
}

func TestUseDirectly(t *testing.T) {
	db, cleanup, err := NewDB()
	if err != nil {
		t.Fatalf("unexpected error (new db): %+v", err)
	}
	defer func() {
		if err := cleanup(); err != nil {
			t.Errorf("unexpected error (close db): %+v", err)
		}
	}()

	// use db
	if db == nil {
		t.Errorf("must not be nil")
	}
}

func NewTestDB(t *testing.T, options ...Option) *DB {
	t.Helper()
	db, cleanup, err := NewDB(options...)
	if cleanup != nil {
		t.Cleanup(func() {
			if err := cleanup(); err != nil {
				t.Errorf("unexpected error (close db): %+v", err)
			}
		})
	}
	if err != nil {
		t.Fatalf("unexpected error (new db): %+v", err)
	}
	return db
}

func TestUseWithFixture(t *testing.T) {
	db := NewTestDB(t)

	// use db
	if db == nil {
		t.Errorf("must not be nil")
	}
}
