package minideps

import "sync"

type producer struct {
	once sync.Once
	Fn   func(state State)
}

func (p *producer) Produce(state State) {
	p.once.Do(func() { p.Fn(state) })
}

// NewProducer :
func NewProducer(fn func(state State)) func(state State) {
	p := &producer{Fn: fn}
	return p.Produce
}

// State :
type State struct {
	Activated bool
}

// consumer :
type consumer struct {
	State *State
	Fn    func(state State)
}

func (c *consumer) Consume(opt func(state *State), depends ...func(state State)) {
	opt(c.State)
	for i := range depends {
		depends[i](*c.State)
	}
	c.Fn(*c.State)
}

// NewConsumer :
func NewConsumer(fn func(state State)) func(opt func(state *State), depends ...func(state State)) {
	c := &consumer{Fn: fn, State: &State{Activated: true}}
	return c.Consume
}

// WithDeactivated :
func WithDeactivated() func(state *State) {
	return func(state *State) {
		state.Activated = false
	}
}

// WithActivated :
func WithActivated() func(state *State) {
	return func(state *State) {
		state.Activated = true
	}
}
