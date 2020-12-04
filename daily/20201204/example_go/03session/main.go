package main

import (
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/k0kubun/pp"
)

type Counter map[string]int

type Session struct {
	Prefix      string
	Measurement string

	TagKeys []string

	C Counter
}

func (s *Session) Add(name string, v int) {
	s.C[name] += v
}

// graphite
type Metric struct {
	Path      string
	Value     string
	Timestamp int64
}

// influxDB
type Point struct {
	Name   string
	Tags   map[string]string
	Fields map[string]interface{}
	Now    time.Time
}

func (s *Session) AsGraphiteData(c Counter) []Metric {
	timestamp := time.Now().Unix()
	prefix := fmt.Sprintf("%s.%s", s.Prefix, s.Measurement)

	metrics := make([]Metric, 0, len(c))
	for key, value := range c {
		path := fmt.Sprintf("%s.%s", prefix, key)
		metrics = append(metrics, Metric{
			Path:      path,
			Value:     strconv.Itoa(value),
			Timestamp: timestamp,
		})
	}
	return metrics
}

func (s *Session) AsInfluxDBData(c Counter) []Point {
	now := time.Now()

	points := make([]Point, 0, len(c))
	for key, value := range c {
		var tags map[string]string
		if len(s.TagKeys) > 0 {
			tags := map[string]string{}
			tagValues := strings.Split(key, ".")
			for i, k := range s.TagKeys {
				tags[k] = tagValues[i]
			}
		}
		points = append(points, Point{
			Name:   s.Measurement,
			Tags:   tags,
			Fields: map[string]interface{}{"count": value}, // 他にtagを追加できないのが問題。
			Now:    now,
		})
	}
	return points
}

func main() {
	s := &Session{
		Prefix:      "monitor",
		Measurement: "jobStatus",
		TagKeys:     []string{"service", "status"},
	}

	s.Add("foo.normal", 10)
	s.Add("foo.internalError", 1)

	// graphite
	pp.Println(s.AsGraphiteData(c))

	// influxDB
	pp.Println(s.AsInfluxDBData(c))
}
