package minideps

import "sync"

// New :
func New() (*Registry, func()) {
	f := &Registry{registered: map[bool][]func(){}}
	return f, f.Start
}

// Registry :
type Registry struct {
	registered map[bool][]func()
}

// Start :
func (r *Registry) Start() {
	for _, status := range []bool{true, false} {
		for _, fn := range r.registered[status] {
			fn()
		}
	}
}

// NewProducer :
func (r *Registry) NewProducer(fn func(state State)) func(state State) {
	p := &producer{Fn: fn, registry: r}
	return p.Produce
}

// NewConsumer :
func (r *Registry) NewConsumer(fn func(state State)) func(opt func(state *State), depends ...func(state State)) {
	c := &consumer{registry: r, Fn: fn, State: &State{Activated: true}}
	return c.Consume
}

// WithDeactivated :
func (r *Registry) WithDeactivated() func(state *State) {
	return func(state *State) {
		state.Activated = false
	}
}

// WithActivated :
func (r *Registry) WithActivated() func(state *State) {
	return func(state *State) {
		state.Activated = true
	}
}

type producer struct {
	registry *Registry
	once     sync.Once
	Fn       func(state State)
}

func (p *producer) Produce(state State) {
	p.once.Do(func() { p.Fn(state) })
}

// State :
type State struct {
	Activated bool
}

// consumer :
type consumer struct {
	registry *Registry
	State    *State
	Fn       func(state State)
}

func (c *consumer) Consume(opt func(state *State), depends ...func(state State)) {
	opt(c.State)
	c.registry.registered[c.State.Activated] = append(
		c.registry.registered[c.State.Activated],
		func() {
			for i := range depends {
				depends[i](*c.State)
			}
			c.Fn(*c.State)
		},
	)
}
