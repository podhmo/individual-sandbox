package findcaller

import (
	"bufio"
	"bytes"
	"strings"
)

// Trace ...
type Trace struct {
	Header string
	Lines  []string
}

// ParseTrace parses stack-trace string
func ParseTrace(trace []byte) ([]*Trace, error) {
	scanner := bufio.NewScanner(bytes.NewBuffer(trace))
	var traces []*Trace
	current := &Trace{}

	for scanner.Scan() {
		line := scanner.Text()
		// goroutine 20 [running]:
		if strings.HasPrefix(line, "goroutine ") && strings.HasSuffix(line, "]:") {
			current.Header = line
		} else if line == "" {
			traces = append(traces, current)
			current = &Trace{}
		} else {
			current.Lines = append(current.Lines, line)
		}
	}

	if len(traces) == 0 || (len(traces) > 0 && traces[len(traces)-1] != current) {
		traces = append(traces, current)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return traces, nil
}
