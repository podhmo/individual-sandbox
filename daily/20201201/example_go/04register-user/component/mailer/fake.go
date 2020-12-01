package mailer

import (
	"fmt"
	"io"
	"strings"
)

type FakeMailer struct {
	W io.Writer
}

func (m *FakeMailer) SendMail(title string, content string, dests []string) error {
	fmt.Fprintln(m.W, "send mail")
	fmt.Fprintf(m.W, "\ttitle: %s\n", title)
	for _, dest := range dests {
		fmt.Fprintf(m.W, "\t\tto: %s\n", dest)
	}
	fmt.Fprintln(m.W, "\t----------------------------------------")
	for _, line := range strings.Split(content, "\n") {
		fmt.Fprintln(m.W, "\t", line)
	}
	return nil
}
