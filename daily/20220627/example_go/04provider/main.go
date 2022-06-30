package main

import "net/http"

type DB struct{}
type JobQueue struct{}

type ProviderA interface {
	DB() *DB
	JobQueue() *JobQueue
}
func mountA[P ProviderA](router *http.ServeMux, getProvider func() P) {
	router.Handle("/foo", Foo(getProvider))
}
func Foo[P ProviderA](getProvider func() P) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {

	})
}

type ProviderB interface {
	AnotherDB() *DB
	JobQueue() *JobQueue
}
func mountB[P ProviderB](router *http.ServeMux, getProvider func() P) {
	router.Handle("/bar", Bar(getProvider))
}
func Bar[P ProviderB](getProvider func() P) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {

	})
}


func main() {
	type provider interface {
		ProviderA
		ProviderB
	}
	getProvider := func() provider {return nil}

	router := &http.ServeMux{}
	mountA(router, getProvider)
	mountB(router, getProvider)
}
