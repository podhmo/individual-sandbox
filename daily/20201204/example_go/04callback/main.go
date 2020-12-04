package main

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/k0kubun/pp"
	"github.com/wacul/ulog"
)

type Event struct {
	DBName      string
	Measurement string
	TagKeys     []string
	Prefix      string
	PrefixKey   string
}

type Counter map[string]int
type Session struct {
	C         Counter
	callbacks []OnFunc
}
type OnFunc func(context.Context, Event, Counter) error

func (s *Session) Add(name string, v int) {
	s.C[name] += v
}
func (s *Session) On(cb OnFunc) {
	s.callbacks = append(s.callbacks, cb)
}

func (s *Session) Commit(ctx context.Context, ev Event) error {
	for _, cb := range s.callbacks {
		if err := cb(ctx, ev, s.C); err != nil {
			ulog.Logger(ctx).WithField("error", err).Warn("on commit")
		}
	}
	return nil
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

func SendToGraphite(ctx context.Context, ev Event, c Counter) error {
	timestamp := time.Now().Unix()
	var prefix string
	if ev.Prefix != "" {
		prefix = fmt.Sprintf("%s.%s.%s", ev.DBName, ev.Prefix, ev.Measurement)
	} else {
		prefix = fmt.Sprintf("%s.%s", ev.DBName, ev.Measurement)
	}

	metrics := make([]Metric, 0, len(c))
	for key, value := range c {
		path := fmt.Sprintf("%s.%s", prefix, key)
		metrics = append(metrics, Metric{
			Path:      path,
			Value:     strconv.Itoa(value),
			Timestamp: timestamp,
		})
	}
	pp.Println(metrics)
	return nil
}

func SendToInfluxDB(ctx context.Context, ev Event, c Counter) error {
	now := time.Now()

	points := make([]Point, 0, len(c))
	for key, value := range c {
		tags := map[string]string{}
		if ev.Prefix != "" {
			tags[ev.PrefixKey] = ev.Prefix
		}
		if len(ev.TagKeys) > 0 {
			tagValues := strings.Split(key, ".")
			for i, k := range ev.TagKeys {
				tags[k] = tagValues[i]
			}
		}
		points = append(points, Point{
			Name:   ev.Measurement,
			Tags:   tags,
			Fields: map[string]interface{}{"count": value}, // 他にtagを追加できないのが問題。
			Now:    now,
		})
	}
	pp.Println(points)
	return nil
}

func main() {
	ctx := context.Background()
	ev := Event{
		DBName:      "monitor",
		Measurement: "jobStatus",
		TagKeys:     []string{"service", "status"},
		Prefix:      "dev",
		PrefixKey:   "env",
	}
	s := &Session{C: Counter{}}
	defer s.Commit(ctx, ev)

	s.On(SendToInfluxDB)
	s.On(SendToGraphite)

	s.Add("foo.normal", 10)
	s.Add("foo.internalError", 1)
}
