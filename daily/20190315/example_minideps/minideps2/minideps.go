package minideps2

import (
	"sync"
)

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
		State:   &State{Name: name, Activated: true},
	}
	g.seen[node.State] = node
	g.nodes = append(g.nodes, node)
	return node
}

// Run :
func (g *Graph) Run() {
	candidates := map[bool][]*Node{}
	seen := map[*Node]struct{}{}

	for _, n := range g.nodes {
		n := n
		if len(n.Depends) > 0 {
			candidates[n.State.Activated] = append(candidates[n.State.Activated], n)
		}
	}

	for _, activated := range []bool{true, false} {
		for _, n := range candidates[activated] {
			for _, p := range n.Depends {
				p.once.Do(func() {
					p.State.Activated = n.State.Activated
					p.Fn(*p.State)
					seen[p] = struct{}{}
				})
			}
			n.once.Do(func() {
				n.Fn(*n.State)
				seen[n] = struct{}{}
			})
		}
	}

	// orphan nodes
	for _, n := range g.nodes {
		if _, ok := seen[n]; !ok {
			n.once.Do(func() {
				n.Fn(*n.State)
			})
		}
	}
}

// State :
type State struct {
	Name      string
	Activated bool
}

// Node :
type Node struct {
	Depends []*Node
	State   *State

	once sync.Once
	Fn   func(state State)
}

// Disabled :
func (n *Node) Disabled() {
	n.State.Activated = false
}
