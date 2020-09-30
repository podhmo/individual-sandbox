package main

import (
	"fmt"
	"net/http"
)

func main() {
	{
		req, _ := http.NewRequest("GET", "http://localhost:8888", nil)
		fmt.Println(req.URL.String())
	}
	{
		req, _ := http.NewRequest("GET", "http://localhost:8888", nil)
		q := req.URL.Query()
		q.Add("pretty", "1")
		req.URL.RawQuery = q.Encode()
		fmt.Println(req.URL.String())
	}
	{
		req, _ := http.NewRequest("GET", "http://localhost:8888", nil)
		q := req.URL.Query()
		q.Add("pretty", "1")
		req.URL.RawQuery = q.Encode()
		fmt.Println(req.URL.String())
	}
}
