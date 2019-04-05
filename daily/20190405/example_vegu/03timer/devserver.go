// +build ignore

package main

import (
	"log"
	"net/http"
	"os"

	"github.com/vugu/vugu/simplehttp"
)

func main() {
	wd, _ := os.Getwd()
	l := "127.0.0.1:8844"
	log.Printf("Starting HTTP Server at %q", l)
	h := simplehttp.New(wd, true)
	// include a CSS file
	// simplehttp.DefaultStaticData["CSSFiles"] = []string{ "/my/file.css" }
	log.Fatal(http.ListenAndServe(l, h))
}
