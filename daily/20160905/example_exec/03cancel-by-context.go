package main

import (
	"bytes"
	"context"
	"log"
	"os/exec"
	"time"
)

// cancel by context

func capture(fn func()) {
	st := time.Now()
	fn()
	defer log.Println("-- ", time.Now().Sub(st))
}

func doSomething(ctx context.Context, waitTime string) {
	var stdout bytes.Buffer

	capture(func() {
		cmd := exec.CommandContext(ctx, "sleep", waitTime)
		cmd.Stdout = &stdout
		cmd.Start()
		cmd.Wait()
		log.Println(cmd.Stdout, cmd.ProcessState)
	})
}

func main() {
	{
		t := 1 * time.Second
		ctx := context.Background()
		ctx, cancel := context.WithTimeout(ctx, t)
		_ = cancel
		doSomething(ctx, "2")
	}
    log.Println("----------------------------------------")
	{
		t := 3 * time.Second
		ctx := context.Background()
		ctx, cancel := context.WithTimeout(ctx, t)
		_ = cancel
		doSomething(ctx, "2")
	}
}
