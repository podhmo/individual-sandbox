package minideps2

import "context"

// New :
func New() (*Graph, func(opts ...func(*Graph))) {
	g := &Graph{
		nodes: []*Node{},
	}
	return g, g.Run
}

// Graph :
type Graph struct {
	nodes []*Node
	ctx   context.Context
	wrap  func(s State, next func(State))
}

// NewNode :
func (g *Graph) NewNode(name string, fn func(state State), opts ...func(*Node)) *Node {
	node := &Node{
		Fn:    fn,
		State: &State{Name: name, Disabled: false},
	}
	g.nodes = append(g.nodes, node)
	for _, op := range opts {
		op(node)
	}
	return node
}

// WithDisabled :
func (g *Graph) WithDisabled() func(n *Node) {
	return func(n *Node) {
		n.State.Disabled = true
	}
}

// WithDepends :
func (g *Graph) WithDepends(nodes ...*Node) func(n *Node) {
	return func(n *Node) {
		n.State.Depends = append(n.State.Depends, nodes...)
	}
}

func (g *Graph) fixDisabled() {
	// propagate disable
	candidates := map[bool][]*Node{}
	for _, n := range g.nodes {
		n := n
		if len(n.State.Depends) > 0 {
			candidates[n.State.Disabled] = append(candidates[n.State.Disabled], n)
		}
	}

	var propagate func(node *Node, isDisabled bool)
	seen := map[*State]struct{}{}
	propagate = func(node *Node, isDisabled bool) {
		seen[node.State] = struct{}{}
		node.State.Disabled = isDisabled
		for _, p := range node.State.Depends {
			propagate(p, isDisabled)
		}
	}

	for _, isDisabled := range []bool{true, false} {
		for _, n := range candidates[isDisabled] {
			if _, ok := seen[n.State]; !ok {
				propagate(n, isDisabled)
			}
		}
	}
}

// Walk :
func (g *Graph) Walk() <-chan *Node {
	g.fixDisabled()

	ch := make(chan *Node)

	go func() {
		defer close(ch)
		candidates := map[bool][]*Node{}
		seen := map[*State]struct{}{}

		for _, n := range g.nodes {
			n := n
			if len(n.State.Depends) > 0 {
				candidates[n.State.Disabled] = append(candidates[n.State.Disabled], n)
			}
		}

		for _, isDisabled := range []bool{false, true} {
			for _, n := range candidates[isDisabled] {
				for _, p := range n.State.Depends {
					if _, ok := seen[p.State]; !ok {
						seen[p.State] = struct{}{}
						ch <- p
					}
				}
				if _, ok := seen[n.State]; !ok {
					seen[n.State] = struct{}{}
					ch <- n
				}
			}
		}

		// orphan nodes
		for _, n := range g.nodes {
			if _, ok := seen[n.State]; !ok {
				seen[n.State] = struct{}{}
				ch <- n
			}
		}
	}()
	return ch
}

// Run :
func (g *Graph) Run(opts ...func(*Graph)) {
	for _, op := range opts {
		op(g)
	}

	wrap := g.wrap
	if wrap == nil {
		wrap = func(s State, next func(State)) {
			next(s)
		}
	}
	for n := range g.Walk() {
		n.State.ctx = g.ctx
		wrap(*n.State, n.Fn)
	}
}

// WithContext :
func WithContext(ctx context.Context) func(*Graph) {
	return func(g *Graph) {
		if ctx == nil {
			panic("nil context")
		}
		g.ctx = ctx
	}
}

// WithWrapFunction :
func WithWrapFunction(wrap func(s State, next func(State))) func(*Graph) {
	return func(g *Graph) {
		g.wrap = wrap
	}
}

// State :
type State struct {
	Name     string
	Disabled bool
	Depends  []*Node
	ctx      context.Context
}

// Context :
func (s *State) Context() context.Context {
	if s.ctx != nil {
		return s.ctx
	}
	return context.Background()
}

// Node :
type Node struct {
	State *State
	Fn    func(state State)
}

// Adjust :
func (n *Node) Adjust(opts ...func(node *Node)) {
	for _, op := range opts {
		op(n)
	}
}
