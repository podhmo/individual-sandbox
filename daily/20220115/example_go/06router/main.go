package main

import "net/http"

type Router[T any] interface {
	Get(path string, action func(http.ResponseWriter, *http.Request) (T, error))
}

// dows not work
/*
func mount[T any](r Router[T]){
	r.Get("/ints", func(http.ResponseWriter, *http.Request) (int, error) { return 10, nil} )
}
*/

func main(){
}
