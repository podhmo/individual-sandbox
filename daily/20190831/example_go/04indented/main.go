package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
)

type withPrefixWriter struct {
	Prefix string
	Writer io.Writer
}

func (w *withPrefixWriter) Write(b []byte) (int, error) {
	buf := bytes.NewBuffer(b)
	s := bufio.NewScanner(buf)

	var total int
	for s.Scan() {
		n, err := io.WriteString(w.Writer, w.Prefix)
		if err != nil {
			return total + n, err
		}
		m, err := w.Writer.Write(s.Bytes())
		if err != nil {
			return total + n + m, err
		}
		_, err = io.WriteString(w.Writer, "\n")
		if err != nil {
			return total + n + m + 1, err
		}
		total += n + m + 1
	}
	return total, nil
}

func main() {
	text := `----------------------------------------
message
----------------------------------------`
	fmt.Println("hello")
	w := &withPrefixWriter{Writer: os.Stdout, Prefix: "\t"}
	fmt.Fprintln(w, text)
}
