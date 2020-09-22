package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func main() {
	addr := os.Getenv("Addr")
	if addr == "" {
		addr = ":4444"
	}
	port, err := strconv.Atoi(addr[1:])
	if err != nil {
		// 多くなってきたらmain()とrun()に分けるべきかも
		log.Fatalf("!%+v", err)
	}

	sentinel := os.Getenv("SENTINEL")
	if sentinel != "" {
		Config{
			URL: fmt.Sprintf("http://localhost:%d/ping", port),
		}.Run(context.Background(), sentinel)
	}

	r := chi.NewRouter()
	r.Use(middleware.Logger)
	r.Use(middleware.Heartbeat("/ping"))

	r.Get("/api", func(w http.ResponseWriter, r *http.Request) {
		data := map[string]string{
			"message": "hello",
		}
		render.JSON(w, r, data)
	})

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Config struct {
	URL       string
	Durations []time.Duration

	Waiter *Waiter
	Logger *log.Logger
}

func (c Config) Run(ctx context.Context, sentinel string) {
	url := c.URL
	if c.Waiter == nil {
		c.Waiter = &Waiter{
			Check:     HealthCheck(url, fmt.Errorf("fail")),
			Durations: DurationsFromSecs([]float64{0.1, 0.2, 0.2, 0.4, 0.8, 1.6, 3.2, 6.4, 12.8}),
		}
	}

	ch := c.Waiter.Start(ctx)

	go func() {
		t := <-ch
		c.Release(sentinel, t)
	}()
}

func (c Config) Release(sentinel string, d time.Duration) {
	if err := os.Remove(sentinel); err != nil {
		c.Logger.Println("ng", d, err)
		return
	}
	c.Logger.Println("ok", d, sentinel)
}

type Waiter struct {
	Check     func(time.Time) error
	Durations []time.Duration

	Logger *log.Logger
}

func (w *Waiter) Start(ctx context.Context) <-chan time.Duration {
	finishCH := make(chan time.Duration)
	go func() {
		defer close(finishCH)
		st := time.Now()

		defer func() {
			panicErr := recover()
			if panicErr != nil {
				log.Fatalf("!!%+v", panicErr)
			}
		}()

		for _, duration := range w.Durations {
			select {
			case <-ctx.Done():
				return
			case t := <-time.After(duration):
				if err := w.Check(t); err != nil {
					continue
				}
				finishCH <- time.Now().Sub(st)
			}
		}

		// timeout
		log.Printf("timeout: %s", time.Now().Sub(st))
	}()
	return finishCH
}

func (w *Waiter) Wait(ctx context.Context) {
	<-w.Start(ctx)
}

func HealthCheck(url string, failErr error) func(time.Time) error {
	return func(t time.Time) error {
		res, err := http.Get(url)
		if err != nil {
			return err
		}
		defer res.Body.Close()
		if res.StatusCode != 200 {
			return fmt.Errorf("unexpeted status %d: %w", res.StatusCode, failErr)
		}
		return nil
	}
}

func DurationsFromSecs(secs []float64) []time.Duration {
	r := make([]time.Duration, len(secs))
	for i, sec := range secs {
		r[i] = time.Duration(sec*1000) * time.Millisecond
	}
	return r
}
