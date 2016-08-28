package main

// http://go-talks.appspot.com/github.com/matryer/present/idiomatic-go-tricks/main.slide#15

type Sizer interface {
	Size() int64
}

func Fits(capacity int64, v Sizer) bool {
	return capacity > v.Size()
}

func IsEmailable(v Sizer) bool {
	return 1<<20 > v.Size()
}

func (f *File) Size() int64 {
	return f.info.Size()
}

// Sizers is also Sizer
type Sizers []Sizer

func (s Sizers) Size() int64 {
	var total int64
	for _, sizer := range s {
		total += sizer.Size()
	}
	return total
}

// or simple version
type SizerFunc func() int64

func (s SizerFunc) Size() int64 {
	return s()
}

// with type alias

type MySize int64

func (s MySize) Size() int64 {
	return int64(s)
}
