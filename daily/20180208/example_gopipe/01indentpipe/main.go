package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"time"
)

// NewWithPrefixPipe :
func NewWithPrefixPipe(prefix string) func(out io.WriteCloser) *io.PipeWriter {
	return func(out io.WriteCloser) *io.PipeWriter {
		r, w := io.Pipe()
		go func() {
			scanner := bufio.NewScanner(r)
			for scanner.Scan() {
				fmt.Fprintln(out, prefix, scanner.Text())
			}
			fmt.Println("***end***", prefix)
			out.Close()
		}()
		return w
	}
}

func main() {
	withStar := NewWithPrefixPipe("**")
	withSharp := NewWithPrefixPipe("##")
	withatmark := NewWithPrefixPipe("@@")
	w := withStar(os.Stdout)
	w2 := withSharp(w)
	w3 := withatmark(w2)
	io.Copy(w3, os.Stdin)
    w3.Close()
	time.Sleep(1 * time.Second)
}
