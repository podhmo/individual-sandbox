package main

import (
	"fmt"
	"sync"
	"log"
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
	return ObjectID(fmt.Sprintf("%s%d", prefix, c))
}

func main() {
    gen := NewOnMemoryObjectIDGenerator()
    var wg sync.WaitGroup
    wg.Add(6)
    go func(){
        log.Println(gen.ObjectID("x"))
        wg.Done()
    }()
    go func(){
        log.Println(gen.ObjectID("x"))
        wg.Done()
    }()
    go func(){
        log.Println(gen.ObjectID("x"))
        wg.Done()
    }()
    go func(){
        log.Println(gen.ObjectID("x"))
        wg.Done()
    }()
    go func(){
        log.Println(gen.ObjectID("y"))
        wg.Done()
    }()
    go func(){
        log.Println(gen.ObjectID("z"))
        wg.Done()
    }()

    log.Println(gen.ObjectID("x"))
    log.Println(gen.ObjectID("y"))
    log.Println(gen.ObjectID("z"))
    wg.Wait()
}
