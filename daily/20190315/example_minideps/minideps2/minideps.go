package minideps2

// New :
func New() (*Graph, func()) {
	g := &Graph{
		nodes: []*Node{},
	}
	return g, g.Run
}

// Graph :
type Graph struct {
	nodes []*Node
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

func (g *Graph) walk() <-chan *Node {
	ch := make(chan *Node)

	// propagate disable
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
func (g *Graph) Run() {
	g.fixDisabled()
	for n := range g.walk() {
		n.Fn(*n.State)
	}
}

// State :
type State struct {
	Name     string
	Disabled bool
	Depends  []*Node
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
