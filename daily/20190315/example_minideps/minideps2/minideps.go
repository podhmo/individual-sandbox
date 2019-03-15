package minideps2

// New :
func New() (*Graph, func()) {
	g := &Graph{
		nodes: []*Node{},
		seen:  map[*State]*Node{},
	}
	return g, g.Run
}

// Graph :
type Graph struct {
	nodes []*Node
	seen  map[*State]*Node
}

// NewNode :
func (g *Graph) NewNode(name string, fn func(state State), depends ...*Node) *Node {
	node := &Node{
		Fn:      fn,
		Depends: depends,
		State:   &State{Name: name, Disabled: false},
	}
	g.seen[node.State] = node
	g.nodes = append(g.nodes, node)
	return node
}

// Walk :
func (g *Graph) Walk() <-chan *Node {
	ch := make(chan *Node)

	go func() {
		defer close(ch)
		candidates := map[bool][]*Node{}
		seen := map[*State]struct{}{}

		for _, n := range g.nodes {
			n := n
			if len(n.Depends) > 0 {
				candidates[n.State.Disabled] = append(candidates[n.State.Disabled], n)
			}
		}

		for _, activated := range []bool{true, false} {
			for _, n := range candidates[activated] {
				for _, p := range n.Depends {
					if _, ok := seen[p.State]; !ok {
						p.State.Disabled = n.State.Disabled
						seen[p.State] = struct{}{}
						ch <- p
					}
					if _, ok := seen[n.State]; !ok {
						seen[n.State] = struct{}{}
						ch <- n
					}
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
	for n := range g.Walk() {
		n.Fn(*n.State)
	}
}

// State :
type State struct {
	Name     string
	Disabled bool
}

// Node :
type Node struct {
	Depends []*Node
	State   *State
	Fn      func(state State)
}

// Disabled :
func (n *Node) Disabled() {
	n.State.Disabled = true
}
