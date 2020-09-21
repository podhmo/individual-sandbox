package main

import (
	"context"
	"fmt"
	"log"
	"sync"
)

func main() {
	ctx := context.Background()
	if err := run(ctx); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(ctx context.Context) error {
	as := &appSession{}
	defer as.Close() // todo handling error

	if err := f(ctx, as); err != nil {
		return fmt.Errorf("run.f: %w", err)
	}
	if err := g(ctx, as); err != nil {
		return fmt.Errorf("run.g: %w", err)
	}
	return nil
}

func f(ctx context.Context, as *appSession) error {
	s, err := as.NewSession()
	if err != nil {
		return fmt.Errorf("new session: %w", err)
	}
	s.Use("f")
	return nil
}
func g(ctx context.Context, as *appSession) error {
	s, err := as.NewSession()
	if err != nil {
		return fmt.Errorf("new session: %w", err)
	}
	s.Use("g")
	return h(ctx, as)
}
func h(ctx context.Context, sf sessionFactory) error {
	s, err := sf.NewSession()
	if err != nil {
		return fmt.Errorf("new session: %w", err)
	}
	s.Use("h")
	return nil
}

type sessionFactory interface {
	NewSession() (*session, error)
}

type session struct {
	id int
}

func (s *session) Use(text string) {
	fmt.Println("Use", s.id, text)
}

type appSession struct {
	session   *session
	teardowns []func() error
	mu        sync.Mutex
}

func (s *appSession) Close() error {
	// todo: use multi error?
	for _, cb := range s.teardowns {
		cb()
	}
	return nil
}
func (s *appSession) NewSession() (*session, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.session != nil {
		return s.session, nil
	}

	s.session = &session{id: 1}
	s.teardowns = append(s.teardowns, func() error {
		fmt.Println("close")
		return nil
	})
	return s.session, nil
}
