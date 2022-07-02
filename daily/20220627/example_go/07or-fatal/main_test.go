package main

import "testing"

type DB struct{}

func GetDB() (DB, error) { return DB{}, nil }

type Accessor struct {
	t *testing.T
}

func (a *Accessor) OrFatal(ob interface{}, err error) interface{} {
	a.t.Helper()
	if err != nil {
		a.t.Fatalf("hmm: %+v", err)
	}
	return ob
}

// func (a *Accessor) [T any]OrFatal(ob T, err error) T {
// 	a.t.Helper()
// 	if err != nil {
// 		a.t.Fatalf("hmm: %+v", err)
// 	}
// 	return ob
// }

func TestIt(t *testing.T) {
	a := &Accessor{t: t}
	db := a.OrFatal(GetDB()).(DB)

	doSomething(db)
}
