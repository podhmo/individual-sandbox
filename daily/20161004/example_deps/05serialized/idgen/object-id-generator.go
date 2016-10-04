package idgen

import (
	"fmt"
	"sync"
)

// ObjectID is x
type ObjectID string

// ObjectIDGenerator is x
type ObjectIDGenerator interface {
	ObjectID(prefix string) ObjectID
}

type onMemoryObjectIDGenerator struct {
	sync.Mutex
	idMap map[string]int
}

// NewOnMemoryObjectIDGenerator is x
func NewOnMemoryObjectIDGenerator() ObjectIDGenerator {
	idMap := make(map[string]int)
	gen := onMemoryObjectIDGenerator{idMap: idMap}
	return ObjectIDGenerator(&gen)
}

// ObjectID is x
func (g *onMemoryObjectIDGenerator) ObjectID(prefix string) ObjectID {
	g.Lock()
	defer g.Unlock()
	c := g.idMap[prefix]
	g.idMap[prefix]++
	return ObjectID(fmt.Sprintf("%s##%d", prefix, c))
}
