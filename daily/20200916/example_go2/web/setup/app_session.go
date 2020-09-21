package setup

import (
	"m/store"
	"sync"
)

type AppSession struct {
	s *Setup

	teardowns []func() error
	mu        sync.Mutex
}

func (s *AppSession) Close() error {
	// todo: use multi error?
	for _, cb := range s.teardowns {
		cb()
	}
	return nil
}
func (s *AppSession) NewStore() (*store.Store, error) {
	// 今は特にrequestごとにsessionのような形でstoreが欲しくはならない
	return s.s.Store(), nil
}
