package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/cybozu-go/well"
)

func main() {
	l := log.New(os.Stdout, "", log.Lshortfile)

	env := well.NewEnvironment(context.Background())
	env.Go(func(ctx context.Context) error {
		l.Println("### Start A")
		defer l.Println("### End  A")

		for i := 0; i < 5; i++ {
			if err := ctx.Err(); err != nil {
				fmt.Println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
				return nil
			}

			l.Println("A", i)
			time.Sleep(200 * time.Millisecond)
		}
		return nil
	})

	env.Go(func(ctx context.Context) error {
		l.Println("### Start B")
		defer l.Println("### End  B")

		for i := 0; i < 10; i++ {
			if err := ctx.Err(); err != nil {
				fmt.Println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
				return nil
			}

			l.Println("B", i)
			time.Sleep(100 * time.Millisecond)
		}
		return nil
	})

	env.Stop()

	if err := env.Wait(); err != nil {
		l.Fatalf("!! %+v", err)
	}
	l.Println("ok")
}
