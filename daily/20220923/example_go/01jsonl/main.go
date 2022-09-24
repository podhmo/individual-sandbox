package main

import (
	"context"
	"encoding/json"
	"io"
	"log"
	"net/http"
	"net/http/httputil"
	"time"
)

func main() {
	addr := ":8008"
	log.Println("listen ...", addr)
	http.ListenAndServe(addr, http.HandlerFunc(handler))
}

type Message struct {
	Message string `json:"message"`
	I       int    `json:"i"`
}

// error処理一切なし
func handler(w http.ResponseWriter, req *http.Request) {
	ctx, cancel := context.WithCancel(req.Context())
	w.Header().Set("Content-Type", "application/x-jsonl")
	cw := httputil.NewChunkedWriter(w)
	flusher, _ := w.(http.Flusher)

	enc := json.NewEncoder(cw)
	go func() {
		defer cancel()
		for i := 0; i < 10; i++ {
			select {
			case <-ctx.Done():
				log.Println("stop")
				return
			case <-time.After(100 * time.Millisecond):
				enc.Encode(Message{I: i, Message: "hello"})
				io.WriteString(cw, "\n")
				flusher.Flush()
			}
		}
		log.Println("end")
	}()
	<-ctx.Done()
	log.Println("ok")
}
