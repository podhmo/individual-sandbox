package g

import "time"

func F(n int) int {
	return f(n) + f(n)
}
func f(n int) int {
	return n + 1
}

func G(now time.Time) time.Time {
	return now
}

type S struct{}

func (s *S) F(n int) int {
	return s.f(n) + s.f(n)
}
func (s *S) f(n int) int {
	return n + 1
}

func (s *S) G(now time.Time) time.Time {
	return now
}
