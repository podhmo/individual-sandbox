package internal

import "sync"

// LazyString :
type LazyString struct {
	ToString func() string

	result string
	once   sync.Once
}

// String
func (ls *LazyString) String() string {
	ls.once.Do(func() {
		ls.result = ls.ToString()
	})
	return ls.result
}

// NewLazyString :
func NewLazyString(toString func() string) *LazyString {
	return &LazyString{
		ToString: toString,
	}
}
