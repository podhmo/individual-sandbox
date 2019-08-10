package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"time"
)

// StdLogger :
type StdLogger struct {
	w             io.Writer
	Internal      *log.Logger
	KeysAndValues []interface{}
}

func New() *StdLogger {
	var logger *StdLogger
	w := os.Stdout
	output := &LTSVOutput{W: w, KeyAndValues: func() []interface{} { return logger.KeysAndValues }}
	logger = &StdLogger{w: w, Internal: log.New(output, "", log.LstdFlags|log.Lshortfile)}
	return logger
}

// With :
func (l *StdLogger) With(keysAndValues ...interface{}) *StdLogger {
	w := l.w
	var logger *StdLogger
	output := &LTSVOutput{W: w, KeyAndValues: func() []interface{} { return logger.KeysAndValues }}
	logger = &StdLogger{
		w:             w,
		Internal:      log.New(output, l.Internal.Prefix(), l.Internal.Flags()),
		KeysAndValues: append(l.KeysAndValues, keysAndValues...),
	}
	return logger
}

// Info :
func (l *StdLogger) Info(msg string) {
	l.Internal.Output(2, msg)
}

// LTSVOutput :
type LTSVOutput struct {
	W            io.Writer
	KeyAndValues func() []interface{}
}

func (o *LTSVOutput) Write(p []byte) (n int, err error) {
	keyAndValues := o.KeyAndValues()
	if len(keyAndValues) == 0 {
		return o.W.Write(p)
	}
	if len(p) == 0 {
		return 0, nil
	}
	if p[len(p)-1] == '\n' {
		p = p[:len(p)-1]
	}
	n, err = o.W.Write(p)
	if err != nil {
		return n, err
	}

	for i := 0; i < len(keyAndValues); i += 2 {
		k := keyAndValues[i]
		v := keyAndValues[i+1]
		m, err := fmt.Printf("	%s:%v", k, v)
		n += m
		if err != nil {
			return n, err
		}
	}
	m, err := o.W.Write([]byte{'\n'})
	if err != nil {
		return n, err
	}
	n += m
	return n, nil
}

func main() {
	l := New()
	l.Info("hello")
	l = l.With("x-id", 10, "y-id", 20)
	l.With("now", time.Now()).Info("bye")
}
