package main

import (
	"bufio"
	"fmt"
	"log"
	"os/exec"
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

	if err := cmd.Start(); err != nil {
		return err
	}

	br := bufio.NewReader(r)
	b, _, err := br.ReadLine()
	if err != nil {
		return err
	}
	firstLine := string(b)

	fmt.Println("@", string(firstLine))
	return nil
}
