package main

import (
	"regexp"
	"strings"
)

// MatchFunc :
type MatchFunc func(s string) bool

// And :
func And(ms ...MatchFunc) MatchFunc {
	return func(s string) bool {
		for _, m := range ms {
			if !m(s) {
				return false
			}
		}
		return true
	}
}

// Or :
func Or(ms ...MatchFunc) MatchFunc {
	return func(s string) bool {
		for _, m := range ms {
			if m(s) {
				return true
			}
		}
		return false
	}
}

// Regex :
func Regex(x string) MatchFunc {
	rx := regexp.MustCompile(x)
	return func(y string) bool {
		return rx.Match([]byte(y))
	}
}

// Exact :
func Exact(x string) MatchFunc {
	return func(y string) bool {
		return x == y
	}
}

// Startswith :
func Startswith(x string) MatchFunc {
	return func(y string) bool {
		return strings.HasPrefix(y, x)
	}
}

// Endswith :
func Endswith(x string) MatchFunc {
	return func(y string) bool {
		return strings.HasSuffix(y, x)
	}
}

// Contains :
func Contains(x string) MatchFunc {
	return func(y string) bool {
		return strings.Contains(y, x)
	}
}

func main() {}
