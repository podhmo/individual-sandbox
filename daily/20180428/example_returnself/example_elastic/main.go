package main

import (
	"context"
	"log"
)

type SearchResult struct{}
type scrollService struct {
	size int
}

func (s *scrollService) Do(ctx context.Context) (*SearchResult, error) {
	return nil, nil
}
func (s *scrollService) Size(size int) *scrollService {
	s.size = size
	return s
}

type myScrollService struct {
	*scrollService
}

func (s *myScrollService) Size(size int) {
	s.scrollService.Size(size)
}

type ScrollService interface {
	Do(ctx context.Context) (*SearchResult, error)
	Size(size int)
}

func Fetch(ctx context.Context, s ScrollService, size int) error {
	_, err := s.Size(size).Do(ctx)
	return err
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	s := &myScrollService{scrollService: &scrollService{}}
	return Fetch(context.Background(), s, 10)
}
