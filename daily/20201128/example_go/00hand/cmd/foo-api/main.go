package main

import (
	"fmt"
	"log"
	"m/00hand/action"
	"net/http"
	"os"
	"strconv"

	"github.com/podhmo/tenuki"
)

func main() {
	addr := ":33333"
	if v := os.Getenv("ADDR"); v != "" {
		addr = v
	}

	if err := Run(addr); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Run(addr string) error {
	mux := http.DefaultServeMux
	mux.HandleFunc("/Hello", func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			t := recover()
			if t != nil {
				log.Printf("hmm %+v", t)
				tenuki.Render(w, r).JSON(500, map[string]string{"message": fmt.Sprintf("%s", t)})
			}
		}()
		var input action.HelloInput
		if err := tenuki.DecodeJSON(r.Body, &input); err != nil {
			tenuki.Render(w, r).JSON(400, map[string]string{"message": err.Error()})
			return
		}
		short := false
		if ok, err := strconv.ParseBool(r.URL.Query().Get("short")); err == nil {
			short = ok
		}
		result, err := action.Hello(r.Context(), input, &short)
		if err != nil {
			code := 500
			if x, ok := err.(interface{ StatusCode() int }); ok {
				code = x.StatusCode()
			}
			tenuki.Render(w, r).JSON(code, map[string]string{"message": err.Error()})
			return
		}
		tenuki.Render(w, r).JSON(200, result)
	})
	log.Printf("listen %s...", addr)
	return http.ListenAndServe(addr, mux)
}
