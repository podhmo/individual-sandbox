package main

import (
	"fmt"
	"net/http"
)

func Handler(w http.ResponseWriter, req *http.Request) {
	fmt.Println("foo", req.Header.Get("foo")) // 大文字小文字区別しない
	for k, v := range req.Header {
		fmt.Println(k, v)
	}
	fmt.Fprintln(w, "ok")
}

func main() {
	h := http.HandlerFunc(Handler)
	http.ListenAndServe(":8080", h)
}
