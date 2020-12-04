package main

import (
	"fmt"
	"strings"
	"time"
)

type Counter map[string]int

func main() {
	prefix := "monitor"

	tbName := "jobStatus"
	tagKeys := []string{"service", "status"}

	c := Counter{}
	c["foo.normal"] = 10
	c["foo.internalError"] = 1

	// graphite
	for k, v := range c {
		fmt.Printf("graphite: %s.%s.%s %v %d\n", prefix, tbName, k, v, time.Now().Unix())
	}

	// influxDB
	N := len(tagKeys)
	for k, v := range c {
		tagValues := strings.Split(k, ".")
		tags := make([]string, 0, N+N)
		for i := 0; i < N; i++ {
			tags = append(tags, fmt.Sprintf("%s=%s", tagKeys[i], tagValues[i]))
		}
		fmt.Printf("influxDB: INSERT %s,%s %s=%v %d\n", tbName, strings.Join(tags, ","), "count", v, time.Now().Unix())
	}
}
