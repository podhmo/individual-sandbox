package main

import "fmt"

// S :
type S struct {
	M map[int]int
}

// GetValue :
func (s *S) GetValue(i int) int {
	return s.M[i]
}

// GetPair :
func (s *S) GetPair(i int) (int, bool) {
	// return s.M[i]
	v, ok := s.M[i]
	return v, ok
}

func main() {
	s := &S{M: map[int]int{}}
	fmt.Println(s.GetPair(10))
}
