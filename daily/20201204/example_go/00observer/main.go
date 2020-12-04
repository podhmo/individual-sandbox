package main

import (
	"context"
	"fmt"
	"log"
	"time"
)

type Event struct {
	Name string
}

type Value struct {
	TagKeys   []string
	TagValues []string

	Key   string
	Value interface{}

	Time time.Time
}

type OnEmitFunc func(ctx context.Context, metrics []Value) error

type Emitter struct {
	Callbacks map[Event][]OnEmitFunc
}

func (e *Emitter) On(ev Event, cb OnEmitFunc) {
	e.Callbacks[ev] = append(e.Callbacks[ev], cb)
}
func (e *Emitter) Emit(ctx context.Context, ev Event, metrics []Value) error {
	for _, cb := range e.Callbacks[ev] {
		if err := cb(ctx, metrics); err != nil {
			log.Printf("! %+v", err)
		}
	}
	return nil
}

func main() {
	AfterCreated := Event{Name: "foo"}

	emitter := &Emitter{Callbacks: map[Event][]OnEmitFunc{}}
	emitter.On(AfterCreated, func(ctx context.Context, metrics []Value) error {
		fmt.Println("Count", len(metrics))
		return nil
	})
	emitter.On(AfterCreated, func(ctx context.Context, metrics []Value) error {
		for _, m := range metrics {
			fmt.Printf("INSERT metrics %s=%v	%d;\n", m.Key, m.Value, m.Time.Unix())
		}
		return nil
	})

	var metrics []Value
	metrics = append(metrics, Value{Key: "foo", Value: 100, Time: time.Now()})
	metrics = append(metrics, Value{Key: "bar", Value: 110, Time: time.Now()})
	metrics = append(metrics, Value{Key: "boo", Value: 200, Time: time.Now()})

	ctx := context.Background()
	emitter.Emit(ctx, AfterCreated, metrics)
}
