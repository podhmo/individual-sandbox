package findcaller

import (
	"fmt"
	"strings"
)

// Option ...
type Option struct {
	Nth   int
	Depth int
}

// WithDepth ...
func WithDepth(depth int) func(*Option) {
	return func(o *Option) {
		o.Depth = depth
	}
}

// WithNth ...
func WithNth(nth int) func(*Option) {
	return func(o *Option) {
		o.Nth = nth
	}
}

// FindCallerFromStackTrace : StackTraceから呼び出しての位置を取得する
func FindCallerFromStackTrace(stackTrace []byte, options ...func(o *Option)) (string, error) {
	traceSet, err := ParseTrace(stackTrace)
	o := &Option{
		Nth:   0,
		Depth: 0,
	}
	for _, opt := range options {
		opt(o)
	}

	if err != nil {
		return "", err
	}
	if len(traceSet) <= o.Nth {
		return "", fmt.Errorf("invalid Nth, %d < %d", len(traceSet), o.Nth)
	}

	i := (o.Depth * 2) + 1
	if len(traceSet[o.Nth].Lines) <= i {
		return "", fmt.Errorf("invalid depth, %d < %d, depth=%d", len(traceSet[o.Nth].Lines), i, o.Depth)
	}
	return strings.TrimPrefix(traceSet[o.Nth].Lines[i], "	"), nil
}
