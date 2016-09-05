package main

// sync call external command (with pipe)

import (
	"bytes"
	"io"
	"log"
	"os/exec"
	"regexp"
)

func main() {
	// jot 10 | grep "2\|4\|6\|8\|10" | tail -r | tr "\n" ","

	cmd0 := exec.Command("jot", "10")
	cmd1 := exec.Command("grep", regexp.QuoteMeta("2|4|6|8|10"))
	cmd2 := exec.Command("tail", "-r")
	cmd3 := exec.Command("tr", "\n", ",")

	r01, w01 := io.Pipe()
	r12, w12 := io.Pipe()
	r23, w23 := io.Pipe()

	cmd0.Stdout = w01
	cmd1.Stdin = r01
	cmd1.Stdout = w12
	cmd2.Stdin = r12
	cmd2.Stdout = w23
	cmd3.Stdin = r23
	var stdout bytes.Buffer
	cmd3.Stdout = &stdout

	cmd0.Start()
	cmd1.Start()
	cmd2.Start()
	cmd3.Start()

	cmd0.Wait()
	w01.Close()

	cmd1.Wait()
	w12.Close()

	cmd2.Wait()
	w23.Close()

	cmd3.Wait()
	log.Println(stdout.String())
}
