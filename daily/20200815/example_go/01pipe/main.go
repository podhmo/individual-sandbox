package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
)

func main() {
	pr, pw := io.Pipe()
	go func() {
		// close the writer, so the reader knows there's no more data
		defer pw.Close()

		hello := bytes.NewBufferString(`
hello
hello
hello
`)
		byebye := bytes.NewBufferString(`
byebye
byebye
byebye
`)
		io.Copy(pw, hello)
		io.Copy(pw, byebye)
	}()

	s := bufio.NewScanner(pr)
	for s.Scan() {
		line := s.Text()
		if line == "" {
			continue
		}
		fmt.Println("*", line)
	}
	if err := s.Err(); err != nil {
		panic(err)
	}
}
