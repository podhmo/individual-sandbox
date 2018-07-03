package main

import (
	"bufio"
	"context"
	"fmt"
	"log"
	"os/exec"
	"time"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	xs := []int{1, 2, 3, 4, 5}
	_ = xs
	cmd := exec.Command("go", "run", "../01showport/main.go")
	r, err := cmd.StdoutPipe()
	if err != nil {
		return err
	}
	br := bufio.NewReader(r)

	if err := cmd.Start(); err != nil {
		return err
	}

	ctx := context.Background()
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()

	var firstLine string
	go func() {
		b, _, innerErr := br.ReadLine()
		if innerErr != nil {
			err = innerErr
		}
		firstLine = string(b)
		cancel()
	}()

	<-ctx.Done()
	if err != nil {
		return err
	}
	if err := ctx.Err(); err != nil {
		return err
	}

	fmt.Println("@", string(firstLine))
	return nil
}
