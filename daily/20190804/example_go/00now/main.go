package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"time"

	rfc3339 "github.com/podhmo/go-rfc3339"
)

// Now :
func Now(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	encoder := json.NewEncoder(w)
	data := map[string]interface{}{
		"now":     rfc3339.Format(time.Now()),
		"message": "hello",
	}
	if err := encoder.Encode(&data); err != nil {
		fmt.Fprintf(w, `{"message": %q}`, err.Error())
	}
}

func Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", Now)
	return mux
}

func main() {
	log.Fatal(http.ListenAndServe(os.Args[1], Handler()))
}
