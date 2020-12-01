package notificator

import (
	"fmt"
	"io"
)

type FakeNotificator struct {
	W io.Writer
}

func (m *FakeNotificator) NotifyMessage(channel string, content string) error {
	fmt.Fprintln(m.W, "notify message")
	fmt.Fprintln(m.W, "\t- - - - - - - - - - - - - - - - - - - - ")
	fmt.Fprintf(m.W, "\tchannel: %s\n", channel)
	fmt.Fprintln(m.W, "\t- - - - - - - - - - - - - - - - - - - - ")
	fmt.Fprintln(m.W, "\t", content)
	return nil
}
