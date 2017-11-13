package main

import (
	"fmt"
	"sync"
)

// S :
type S struct {
	sync.Mutex
	i int
}

// Inc :
func (s *S) Inc() int {
	s.Lock()
	defer s.Unlock()
	s.i++
	return s.i
}

// IncInc :
func (s *S) IncInc() int {
	s.Lock()
	defer s.Unlock()
	s.i++
	return s.Inc() // go's lock is not reentrant (dead lock)
}

// correct answer

// S2 :
type S2 struct {
	sync.Mutex
	i int
}

func (s *S2) inc() int {
	s.i++
	return s.i
}

// Inc :
func (s *S2) Inc() int {
	s.Lock()
	defer s.Unlock()
	return s.inc()
}

// IncInc :
func (s *S2) IncInc() int {
	s.Lock()
	defer s.Unlock()
	s.inc()
	return s.inc()
}

func main() {
	s := S{}
	fmt.Println(s.Inc())
	fmt.Println(s.IncInc()) // dead lock!
}
