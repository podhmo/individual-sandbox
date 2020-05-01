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

	well.Go(func(ctx context.Context) error {
		l.Println("### Start A")
		defer l.Println("### End  A")

		for i := 0; i < 5; i++ {
			select {
			case <-ctx.Done():
				fmt.Println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
				return nil
			case <-time.After(200 * time.Millisecond):
				l.Println("A", i)
			}
		}
		return nil
	})

	well.Go(func(ctx context.Context) error {
		l.Println("### Start B")
		defer l.Println("### End  B")

		for i := 0; i < 10; i++ {
			select {
			case <-ctx.Done():
				fmt.Println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
				return nil
			case <-time.After(100 * time.Millisecond):
				l.Println("B", i)
			}
		}
		return nil
	})

	// well.Stop()
	// time.Sleep(500 * time.Millisecond)
	// well.Cancel(fmt.Errorf("heh"))

	if err := well.Wait(); err != nil {
		l.Fatalf("!! %+v", err)
	}
}
