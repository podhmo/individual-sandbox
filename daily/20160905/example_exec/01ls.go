package main

// sync call external command

import (
	"bytes"
	"log"
	"os/exec"
)

func main() {
	cmd := exec.Command("ls", "-l")
	var stdout bytes.Buffer
	cmd.Stdout = &stdout

	cmd.Start()
	log.Println("hmm")
	err := cmd.Wait()

	if err != nil {
		log.Fatal(err)
	}
	log.Println(stdout.String())
}
