package main

import (
	"fmt"
	"log"
	"net/http"
	"strconv"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Input struct {
	Size  int64
	Sort  string // "id","-id"
	Debug bool
}

func run() error {
	req, err := http.NewRequest("GET", "/items?sort=-id&size=100", nil)
	if err != nil {
		return err
	}

	q := req.URL.Query()

	input := Input{}
	if v := q.Get("size"); v != "" {
		n, err := strconv.ParseInt(v, 10, 64)
		if err != nil {
			return fmt.Errorf("parse size: %w (value=%v)", err, v)
		}
		input.Size = n
	}
	if v := q.Get("sort"); v != "" {
		switch v {
		case "id", "-id":
			input.Sort = v
		default:
			return fmt.Errorf("parse sort: %w (value=%v)", err, v)
		}
	}

	fmt.Printf("%+v\n", input)
	return nil
}
