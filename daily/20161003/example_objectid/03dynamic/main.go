package main

import (
	"bytes"
	"fmt"
	"log"
	"path"
	"reflect"
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

// ServiceSpec is x
type ServiceSpec struct {
	BasePath            string
	ServiceDependencies []string
	ItemDependencies    []string
	EndNode             bool
}

// ServiceSettings is x
type ServiceSettings map[string]ServiceSpec

// Status is x
type Status int

// JobID is x
type JobID ObjectID

// Endpoint is x
type Endpoint string

const (
	Waiting Status = iota
	Initialized
	Pending
	Cancelled
	Error
	Finished
)

// State is x
type State struct {
	JobID         JobID
	Params        map[string]string // xxx
	Status        Status
	EndNode       bool
	StatusAPIPath Endpoint
}

// Analysis
type Analysis struct {
	targetID string
	xID      string
	yID      string
	zID      string
}

// Sentinel is x
type Sentinel struct {
	endpoints []Endpoint
}

// WholeState is x
type WholeState struct {
	sentinel *Sentinel
	States   map[string]State
	Analysis *Analysis
}

// NewSentinel is x
func NewSentinel() *Sentinel {
	endpoints := []Endpoint{}
	sentinel := Sentinel{endpoints: endpoints}
	return &sentinel
}

// NewSentinelFromStates is x
func NewSentinelFromStates(states map[string]State) *Sentinel {
	sentinel := NewSentinel()
	for _, s := range states {
		if s.EndNode {
			sentinel.endpoints = append(sentinel.endpoints, s.StatusAPIPath)
		}
	}
	return sentinel
}

// NewWholeState is x
func NewWholeState(analysis *Analysis) *WholeState {
	ws := WholeState{States: make(map[string]State), Analysis: analysis}
	return &ws
}

// StateWalker is x
type StateWalker struct {
	gen      ObjectIDGenerator
	Settings *ServiceSettings
}

// GetSpec is x
func (sw *StateWalker) GetSpec(key string) (ServiceSpec, bool) {
	m := (map[string]ServiceSpec)(*sw.Settings)
	spec, ok := m[key]
	return spec, ok
}

// WalkSpec is x
func (sw *StateWalker) WalkSpec(serviceName string, spec ServiceSpec, ws *WholeState) (JobID, error) {
	state, ok := ws.States[serviceName]
	if ok {
		return state.JobID, nil
	}
	be := newBufError()
	v := reflect.ValueOf(ws.Analysis).Elem()

	params := make(map[string]string)

	// item params
	for _, name := range spec.ItemDependencies {
		field := v.FieldByName(name)
		if !field.IsValid() {
			be.WriteString(fmt.Sprintf("%s -> item %s is not found", serviceName, name))
			continue
		}
		params[name] = field.String()
	}

	// service params
	for _, name := range spec.ServiceDependencies {
		spec, ok := sw.GetSpec(name)
		if !ok {
			be.WriteString(fmt.Sprintf("%s -> service %s is not found.\n", serviceName, name))
			continue
		}
		jobID, err := sw.WalkSpec(name, spec, ws)
		if err != nil {
			be.WriteString(fmt.Sprintf("%s -> {%v}\n", serviceName, err))
		}
		params[name] = string(jobID)
	}

	jobID := JobID(sw.gen.ObjectID(serviceName))
	state = State{
		JobID:         jobID,
		Status:        Waiting,
		Params:        params,
		EndNode:       spec.EndNode,
		StatusAPIPath: Endpoint(path.Join(spec.BasePath, "status", string(jobID))),
	}
	ws.States[serviceName] = state
	return jobID, be.Error()
}

// WalkByNames is x
func (sw *StateWalker) WalkByNames(ws *WholeState, required []string) error {
	be := newBufError()
	for _, name := range required {
		spec, ok := sw.GetSpec(name)
		if !ok {
			log.Printf("%s service is not found.\n", name)
			continue
		}
		_, err := sw.WalkSpec(name, spec, ws)
		if err != nil {
			be.WriteString(fmt.Sprintf("%v\n", err))
		}
	}
	ws.sentinel = NewSentinelFromStates(ws.States)
	return be.Error()
}

type bufError struct {
	buf bytes.Buffer
}

func (be *bufError) WriteString(s string) {
	be.buf.WriteString(s)
}
func (be *bufError) Error() error {
	msg := be.buf.String()
	if len(msg) > 0 {
		return fmt.Errorf(msg)
	}
	return nil
}
func newBufError() *bufError {
	var buf bytes.Buffer
	be := bufError{buf: buf}
	return &be
}

func main() {
	sw := StateWalker{Settings: &Settings, gen: NewOnMemoryObjectIDGenerator()}
	analysis := Analysis{
		targetID: "targetId",
		xID:      "xid",
		yID:      "yid",
		zID:      "zid",
	}
	ws := NewWholeState(&analysis)

	// patten 1
	pattern1 := []string{"taskB0", "taskB4", "taskA3"}
	err := sw.WalkByNames(ws, pattern1)
	if err != nil {
		panic(err)
	}
	log.Printf("%#v\n", ws)
}

// Settings is x
var Settings ServiceSettings

/*
task dependencies
targetId -> taskA0 -> taskB0 (end)
targetId, xId -> task1 -> taskB1 (end)
yId -> taskA2 -> taskB2 (end)
xId, yId, zId -> taskA3 (end)
targetId, xId -> taskA4
targetId, yId -> taskA5
taskA4, taskA5 -> taskB3 (end)
taskA0, taskA1, taskA2 -> taskB4(end)
*/

func init() {
	Settings = ServiceSettings{
		"taskA0": ServiceSpec{
			BasePath:            "/api/task/A0/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"targetID"},
			EndNode:             false,
		},
		"taskB0": ServiceSpec{
			BasePath:            "/api/task/B0/",
			ServiceDependencies: []string{"taskA1"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA1": ServiceSpec{
			BasePath:            "/api/task/A1/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"targetID", "xID"},
			EndNode:             false,
		},
		"taskB1": ServiceSpec{
			BasePath:            "/api/task/B1/",
			ServiceDependencies: []string{"taskA1"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA2": ServiceSpec{
			BasePath:            "/api/task/A2/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"yID"},
			EndNode:             false,
		},
		"taskB2": ServiceSpec{
			BasePath:            "/api/task/B2/",
			ServiceDependencies: []string{"taskA2"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskA3": ServiceSpec{
			BasePath:            "/api/task/A3/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"xID", "yID", "zID"},
			EndNode:             true,
		},
		"taskA4": ServiceSpec{
			BasePath:            "/api/task/A4/",
			ServiceDependencies: []string{"targetID", "xID"},
			ItemDependencies:    []string{},
			EndNode:             false,
		},
		"taskA5": ServiceSpec{
			BasePath:            "/api/task/A5/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"targetID", "yID"},
			EndNode:             false,
		},
		"taskB3": ServiceSpec{
			BasePath:            "/api/task/B3/",
			ServiceDependencies: []string{"taskA4", "taskA5"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
		"taskB4": ServiceSpec{
			BasePath:            "/api/task/B4/",
			ServiceDependencies: []string{"taskA0", "taskA1", "taskA2"},
			ItemDependencies:    []string{},
			EndNode:             true,
		},
	}
}
