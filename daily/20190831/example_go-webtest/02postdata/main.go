package main

import (
	"fmt"
	"net/http"
)

func Handler(w http.ResponseWriter, req *http.Request) {
	fmt.Println("foo", req.FormValue("foo"))
	for k, v := range req.Form {
		fmt.Println(k, v)
	}
	fmt.Fprintln(w, "ok")
}

func main() {
	h := http.HandlerFunc(Handler)
	http.ListenAndServe(":8080", h)
}
