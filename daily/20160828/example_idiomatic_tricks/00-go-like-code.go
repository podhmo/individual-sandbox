package main

import (
	"errors"
	"sync"
)

type Thing struct {
	N int
}

var another = struct {
	*sync.Mutex
	Update func(n int) error
	n      int
	Thing  func() *Thing
}{
	n:      0,
	Update: func(n int) error { another.n += n; return nil },
	Thing:  func() *Thing { return &Thing{N: another.n} },
}

type OKer interface {
	OK() bool
}
type Closer interface {
	Close()
}
type OKCloser interface {
	Closer
	OKer
}

func GetSomething() (OKCloser, error) {
	return nil, nil
}

func BrilliantFunction() (*Thing, error) {
	something, err := GetSomething()
	if err != nil {
		return nil, err
	}
	defer something.Close()

	if !something.OK() {
		return nil, errors.New("something whent wrong")
	}

	another.Lock()
	defer another.Unlock()
	err = another.Update(1)
	if err != nil {
		return nil, err
	}
	return another.Thing(), nil
}

/*
func UnbrilliantFunction() (*Thing, error) {
    something, err := GetSomething()
    if err != nil {
        return nil, err
    }
    defer something.Close()
    if something.OK() {
        another, err := something.Else()
        if err != nil {
            return nil, &customErr{err: err, location: "BrilliantFunction"}
        }
        another.Lock()
        err = another.Update(1)
        if err == nil {
            another.Unlock()
            return another.Thing(), nil
        }
        another.Unlock()
        return nil, err
    } else {
        return nil, errors.New("something went wrong")
    }
}
*/
