package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/cenkalti/backoff"
	"github.com/gofiber/fiber"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lmicroseconds)
	port, err := strconv.Atoi(os.Getenv("PORT"))
	if err != nil {
		log.Fatalf("!%+v", err)
	}

	sentinel := os.Getenv("SENTINEL")
	if sentinel != "" {
		go func() {
			defer func() {
				panicErr := recover()
				if panicErr != nil {
					log.Fatalf("!!%+v", panicErr)
				}
			}()

			operation := func() error {
				res, err := http.Get(fmt.Sprintf("http://localhost:%d/", port))
				if err != nil {
					return err
				}
				defer res.Body.Close()
				if res.StatusCode == 200 {
					defer res.Body.Close()
					if err := os.Remove(sentinel); err != nil {
						log.Printf("hmm %v", err)
						return nil
					}
					log.Println("ok", sentinel)
					return nil
				}
				return fmt.Errorf("hmm status=%d", res.StatusCode)
			}

			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			b := backoff.NewExponentialBackOff()
			if err := backoff.Retry(
				operation,
				backoff.WithMaxRetries(backoff.WithContext(b, ctx), 5),
			); err != nil {
				log.Fatalf("!%+v", err)
			}
		}()
	}

	app := fiber.New()

	app.Get("/", func(c *fiber.Ctx) {
		c.Send("Hello, World 👋!")
	})

	app.Listen(port)
}
