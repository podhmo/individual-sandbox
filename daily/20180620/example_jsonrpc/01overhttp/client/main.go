package main

import (
	"bytes"
	"io"
	"log"
	"net/http"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	var b bytes.Buffer
	b.WriteString(`
{
  "method": "Arith.Add",
  "params": [{"A": 10, "B": 20}],
  "jsonrpc": "2.0",
  "id": 0
}`)

	req, err := http.NewRequest("POST", "http://localhost:4000/jsonrpc", &b)
	if err != nil {
		return err
	}
	req.Header.Set("Content-Type", "application/json")

	client := http.DefaultClient
	resp, err := client.Do(req)
	if err != nil {
		return err
	}

	defer resp.Body.Close()
	if _, err := io.Copy(os.Stdout, resp.Body); err != nil {
		return err
	}
	return nil
}
