package main

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/k0kubun/pp"
)

func Handler(w http.ResponseWriter, req *http.Request) {
	defer req.Body.Close()
	decoder := json.NewDecoder(req.Body)
	var ob interface{}
	if err := decoder.Decode(&ob); err != nil {
		panic(err) // xxx
	}
	pp.Println(ob)

	fmt.Fprintln(w, "ok")
}

func main() {
	h := http.HandlerFunc(Handler)
	http.ListenAndServe(":8080", h)
}
