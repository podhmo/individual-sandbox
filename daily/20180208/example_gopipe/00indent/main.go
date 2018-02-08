package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

func main() {
	r, w := io.Pipe()
	go func() {
		scanner := bufio.NewScanner(r)
		for scanner.Scan() {
			fmt.Fprintln(os.Stdout, "**", scanner.Text())
		}
	}()
	io.Copy(w, os.Stdin)
}
