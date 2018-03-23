package internal

import (
	"math/rand"
	"time"
)

// Service :
type Service struct {
	Name  string
	Value int
}

// Update :
func (s *Service) Update() {
	s.Value = s.Value + 1
	time.Sleep(time.Duration(rand.Intn(100)) * time.Millisecond)
}
