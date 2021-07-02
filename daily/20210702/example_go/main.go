package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"
)

func logger(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		log.Print(r.RequestURI)
		h.ServeHTTP(w, r)
	})
}

func health(w http.ResponseWriter, r *http.Request) {}

func main() {
	port := 8080
	flag.IntVar(&port, "p", 8080, "http listen port")
	flag.Parse()
	log.SetFlags(log.Lshortfile)
	ctx := context.Background()
	// ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	// defer stop()
	l, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		log.Fatal(err)
	}
	defer l.Close()
	log.Println("listen:", l.Addr())
	http.HandleFunc("/api/health", health)
	server := &http.Server{Handler: logger(http.DefaultServeMux)}
	go func() {
		if err := server.Serve(l); err != nil {
			log.Fatal(err)
		}
	}()
	<-ctx.Done()
	server.Shutdown(ctx)
}
