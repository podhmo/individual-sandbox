package glue

func NewGlue() *Glue {
	return &Glue{
		path:       []string{"g"},
		groups:     map[string]*Glue{},
		operations: map[string][]*Operation{},
		Helper:     &Helper{},
	}
}

type Glue struct {
	parent *Glue
	path   []string

	groups     map[string]*Glue
	seenGroups []string

	operations     map[string][]*Operation
	seenOperations []string

	Helper *Helper
}

func (g *Glue) Name() string {
	return g.path[len(g.path)-1]
}

func (g *Glue) newChild(name string) *Glue {
	child := &Glue{
		groups:     map[string]*Glue{},
		operations: map[string][]*Operation{},
	}
	child.path = append(g.path[:], name)
	child.parent = g
	child.Helper = g.Helper
	return child
}

func (g *Glue) Group(name string, use func(g *Glue)) {
	if r, ok := g.groups[name]; ok {
		use(r)
		return
	}
	g.seenOperations = append(g.seenOperations, name)
	r := g.newChild(name)
	g.groups[name] = r
	use(r)
}

func (g *Glue) Operation(opID string, fn interface{}, rest ...interface{}) {
	g.seenOperations = append(g.seenOperations, opID)
	r := &Operation{}
	g.operations[opID] = append(g.operations[opID], r)
}

type Operation struct{}

type Helper struct {
}
