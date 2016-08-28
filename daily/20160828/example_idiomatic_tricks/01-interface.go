package main

// http://go-talks.appspot.com/github.com/matryer/present/idiomatic-go-tricks/main.slide#8

// interface

type Reader interface {
	Read(p []byte) (n int, err error)
}

type Handler interface {
	ServeHttp(ResponseWriter, *Request)
}

// but more simple case, func type is used

type HandleFunc func(ResponseWriter, *Request)

// ServeHttp calls f(w, r)
// mapping for HandleFunc to Handler interface.
func (f HandlerFunc) ServeHttp(w ResponseWriter, r *Request){
    f(w, r)
}
