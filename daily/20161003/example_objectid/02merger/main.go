package main

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

/*
task dependencies
targetId -> taskA0 -> taskB0 (end)
targetId, xId -> taskA1 -> taskB1 (end)
yId -> taskA2 -> taskB2 (end)
xId, yId, zId -> taskA3 (end)
targetId, xId -> taskA4
targetId, yId -> taskA5
taskA4, taskA5 -> taskB3 (end)
taskA0, taskA1, taskA2 -> taskB4(end)
*/

// TargetID is x
type TargetID string

// XID is x
type XID string

// YID is x
type YID string

// ZID is x
type ZID string

// TaskA0ID is x
type TaskA0ID ObjectID

// TaskA1ID is x
type TaskA1ID ObjectID

// TaskA2ID is x
type TaskA2ID ObjectID

// TaskA3ID is x
type TaskA3ID ObjectID

// TaskA4ID is x
type TaskA4ID ObjectID

// TaskA5ID is x
type TaskA5ID ObjectID

// TaskB0ID is x
type TaskB0ID ObjectID

// TaskB1ID is x
type TaskB1ID ObjectID

// TaskB2ID is x
type TaskB2ID ObjectID

// TaskB3ID is x
type TaskB3ID ObjectID

// TaskB4ID is x
type TaskB4ID ObjectID

// Status is x
type Status int
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
	JobID   ObjectID
	Params  map[string]string // xxx
	Status  Status
	EndNode bool
    StatusAPIPath Endpoint
}

// WholeState is x
type WholeState struct {
	states  map[string]State
    sentinel *Sentinel
	targetID TargetID
	xID      XID
	yID      YID
	zID      ZID
}

// NewWholeState is x
func NewWholeState() *WholeState {
	ws := WholeState{states: make(map[string]State)}
	return &ws
}

// StateInitializer is x
type StateInitializer struct {
	gen ObjectIDGenerator
}

func (si *StateInitializer) initTaskA0(ws *WholeState) (TaskA0ID, error) {
	// targetID -> taskA0 -> taskB0 (end)
	k := "taskA0"
	state, ok := ws.states[k]
	if ok {
		return TaskA0ID(state.JobID), nil
	}

	if ws.targetID == "" {
		return "", fmt.Errorf("targetId is not found")
	}
	jobID := si.gen.ObjectID("taskA0")
	params := make(map[string]string)
	params["targetId"] = string(ws.targetID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: false,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taska0/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskA0ID(jobID), nil
}
func (si *StateInitializer) initTaskB0(ws *WholeState) (TaskB0ID, error) {
	// taskAID -> taskA0 -> taskB0 (end)
	k := "taskB0"
	state, ok := ws.states[k]
	if ok {
		return TaskB0ID(state.JobID), nil
	}

	params := make(map[string]string)
	taskA0ID, err := si.initTaskA0(ws)
	if err != nil {
		return "", err
	}
	jobID := si.gen.ObjectID("taskB0")
	params["taskA0Id"] = string(taskA0ID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: true,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taskb0/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskB0ID(jobID), nil
}
func (si *StateInitializer) initTaskA1(ws *WholeState) (TaskA1ID, error) {
	// targetId, xId -> taskA1 -> taskB1 (end)
	k := "taskA1"
	state, ok := ws.states[k]
	if ok {
		return TaskA1ID(state.JobID), nil
	}

	targetID := ws.targetID
	if targetID == "" {
		return "", fmt.Errorf("targetId is not found")
	}
	xID := ws.xID
	if xID == "" {
		return "", fmt.Errorf("xId is not found")
	}
	jobID := si.gen.ObjectID("taskA1")
	params := make(map[string]string)
	params["targetId"] = string(targetID)
	params["xId"] = string(xID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: false,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taska1/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskA1ID(jobID), nil
}
func (si *StateInitializer) initTaskB1(ws *WholeState) (TaskB1ID, error) {
	// targetId, xId -> taskA1 -> taskB1 (end)
	k := "taskB1"
	state, ok := ws.states[k]
	if ok {
		return TaskB1ID(state.JobID), nil
	}

	taskA1ID, err := si.initTaskA1(ws)
	if err != nil {
		return "", err
	}
	jobID := si.gen.ObjectID("taskB1")
	params := make(map[string]string)
	params["taskA1Id"] = string(taskA1ID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: true,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taskb1/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskB1ID(jobID), nil
}
func (si *StateInitializer) initTaskA2(ws *WholeState) (TaskA2ID, error) {
	// yId -> taskA2 -> taskB2 (end)
	k := "taskA2"
	state, ok := ws.states[k]
	if ok {
		return TaskA2ID(state.JobID), nil
	}

	yID := ws.yID
	if yID == "" {
		return "", fmt.Errorf("yID is not found")
	}
	jobID := si.gen.ObjectID("taskA2")
	params := make(map[string]string)
	params["yId"] = string(yID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: false,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taska2/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskA2ID(jobID), nil
}
func (si *StateInitializer) initTaskB2(ws *WholeState) (TaskB2ID, error) {
	// targetId, xId -> taskA2 -> taskB2 (end)
	k := "taskB2"
	state, ok := ws.states[k]
	if ok {
		return TaskB2ID(state.JobID), nil
	}

	taskA2ID, err := si.initTaskA2(ws)
	if err != nil {
		return "", err
	}
	jobID := si.gen.ObjectID("taskB2")
	params := make(map[string]string)
	params["taskA2Id"] = string(taskA2ID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: true,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taskb2/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskB2ID(jobID), nil
}
func (si *StateInitializer) initTaskA3(ws *WholeState) (TaskA3ID, error) {
	// xId, yId, zId -> taskA3 (end)
	k := "taskA3"
	state, ok := ws.states[k]
	if ok {
		return TaskA3ID(state.JobID), nil
	}

	xID := ws.xID
	if xID == "" {
		return "", fmt.Errorf("xID is not found")
	}
	yID := ws.yID
	if yID == "" {
		return "", fmt.Errorf("yID is not found")
	}
	zID := ws.zID
	if zID == "" {
		return "", fmt.Errorf("zID is not found")
	}
	jobID := si.gen.ObjectID("taskA3")
	params := make(map[string]string)
	params["xId"] = string(xID)
	params["yId"] = string(yID)
	params["zId"] = string(zID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: true,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taska3/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskA3ID(jobID), nil
}
func (si *StateInitializer) initTaskA4(ws *WholeState) (TaskA4ID, error) {
	// targetId, xId -> taskA4
	k := "taskA4"
	state, ok := ws.states[k]
	if ok {
		return TaskA4ID(state.JobID), nil
	}
	xID := ws.xID
	if xID == "" {
		return "", fmt.Errorf("xID is not found")
	}
	jobID := si.gen.ObjectID("taskA4")
	params := make(map[string]string)
	params["xId"] = string(xID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: false,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taska4/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskA4ID(jobID), nil
}
func (si *StateInitializer) initTaskA5(ws *WholeState) (TaskA5ID, error) {
	// targetId, yId -> taskA5
	k := "taskA5"
	state, ok := ws.states[k]
	if ok {
		return TaskA5ID(state.JobID), nil
	}
	yID := ws.yID
	if yID == "" {
		return "", fmt.Errorf("yID is not found")
	}
	jobID := si.gen.ObjectID("taskA5")
	params := make(map[string]string)
	params["yId"] = string(yID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: false,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taska5/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskA5ID(jobID), nil
}
func (si *StateInitializer) initTaskB3(ws *WholeState) (TaskB3ID, error) {
	// taskA4, taskA5 -> taskB3 (end)
	k := "taskB3"
	state, ok := ws.states[k]
	if ok {
		return TaskB3ID(state.JobID), nil
	}

	taskA4ID, err := si.initTaskA4(ws)
	if err != nil {
		return "", err
	}
	taskA5ID, err := si.initTaskA5(ws)
	if err != nil {
		return "", err
	}
	jobID := si.gen.ObjectID("taskB3")
	params := make(map[string]string)
	params["taskA4ID"] = string(taskA4ID)
	params["taskA5ID"] = string(taskA5ID)

	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: true,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taskb3/status/%s", jobID)),
	}
	ws.states[k] = state
	return TaskB3ID(jobID), nil
}
func (si *StateInitializer) initTaskB4(ws *WholeState) (TaskB4ID, error) {
	// taskA0, taskA1, taskA2 -> taskB4(end)
	k := "taskB4"
	state, ok := ws.states[k]
	if ok {
		return TaskB4ID(state.JobID), nil
	}

	taskA0ID, err := si.initTaskA0(ws)
	if err != nil {
		return "", err
	}
	taskA1ID, err := si.initTaskA1(ws)
	if err != nil {
		return "", err
	}
	taskA2ID, err := si.initTaskA2(ws)
	if err != nil {
		return "", err
	}

	jobID := si.gen.ObjectID("taskB4")
	params := make(map[string]string)
	params["taskA0ID"] = string(taskA0ID)
	params["taskA1ID"] = string(taskA1ID)
	params["taskA2ID"] = string(taskA2ID)
	state = State{
		JobID:   jobID,
		Status:  Waiting,
		Params:  params,
		EndNode: false,
        StatusAPIPath: Endpoint(fmt.Sprintf("/api/taskb4/status/%s", jobID)),
	}
	ws.states["taskB4"] = state
	return TaskB4ID(jobID), nil
}


// Sentinel is x
type Sentinel struct {
    endpoints []Endpoint
}

// NewSentinel is x
func NewSentinel() *Sentinel{
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

func pattern1(si *StateInitializer) {
	ws := NewWholeState()

	ws.targetID = "target0"
	ws.xID = "xId"
	ws.yID = "yId"
	ws.zID = "zId"

	// pattern 1
	if _, err := si.initTaskB0(ws); err != nil {
		panic(err)
	}
	if _, err := si.initTaskB4(ws); err != nil {
		panic(err)
	}
	if _, err := si.initTaskA3(ws); err != nil {
		panic(err)
	}

    sentinel := NewSentinelFromStates(ws.states)
    fmt.Println("----------------------------------------")
	fmt.Printf("%v\n", ws)
    fmt.Println("----------------------------------------")
	fmt.Printf("sentinel %v\n", sentinel)
}
func pattern2(si *StateInitializer) {
	ws := NewWholeState()

	ws.targetID = "target0"
	ws.xID = "xId@"
	ws.yID = "yId@"
	ws.zID = "zId@"

	// pattern 2
	if _, err := si.initTaskB1(ws); err != nil {
		panic(err)
	}
	if _, err := si.initTaskB2(ws); err != nil {
		panic(err)
	}
	if _, err := si.initTaskB3(ws); err != nil {
		panic(err)
	}

    sentinel := NewSentinelFromStates(ws.states)
    fmt.Println("----------------------------------------")
	fmt.Printf("%v\n", ws)
    fmt.Println("----------------------------------------")
	fmt.Printf("sentinel %v\n", sentinel)
}

func main() {
	gen := NewOnMemoryObjectIDGenerator()
	si := StateInitializer{gen: gen}

    var wg sync.WaitGroup
    wg.Add(4)
    go func(){
        pattern1(&si)
        wg.Done()
    }()
    go func(){
        pattern2(&si)
        wg.Done()
    }()
    go func(){
        pattern2(&si)
        wg.Done()
    }()
    go func(){
        pattern2(&si)
        wg.Done()
    }()
    wg.Wait()
}
