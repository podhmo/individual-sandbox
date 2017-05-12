package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

func main() {
	response, err := http.Get("http://localhost:54321")
	if err != nil {
		log.Fatal(err)
	}
	io.Copy(os.Stdout, response.Body)
}
