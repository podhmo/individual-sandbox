package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"path"
	"reflect"
	"sync"
)

// JobID is x
type JobID ObjectID

/**

API examples

- POST /skeleton -- 外部serviceの初期データ生成
- GET /status/{jobId} -- 現在のjobの状態を返す

**/

// SkeletonResponse is x
type SkeletonResponse interface{}

// StatusResponse is x
type StatusResponse struct {
	Status Status
}

// ExternalServiceAccessor is a manager that accessing external service (e.g. taskA0)
type ExternalServiceAccessor interface {
	CreateSkeleton(basePath string, jobID JobID, params map[string]string) (SkeletonResponse, error)
	CreateSkeletonByURL(url string, params map[string]string) (SkeletonResponse, error)
	GetStatus(basePath string, jobID JobID) (StatusResponse, error)
	GetStatusByURL(url string) (StatusResponse, error)
	BuildGetStatusURL(basePath string, jobID JobID) string
}

// DummyAccessor is x
type DummyAccessor struct {
}

// CreateSkeleton is x
func (d *DummyAccessor) CreateSkeleton(basePath string, jobID JobID, params map[string]string) (SkeletonResponse, error) {
	url := path.Join(basePath, "skeleton")
	params["jobId"] = string(jobID)
	return d.CreateSkeletonByURL(url, params)
}

// CreateSkeletonByURL is x
func (d *DummyAccessor) CreateSkeletonByURL(url string, params map[string]string) (SkeletonResponse, error) {
	log.Printf("POST: %s params=%v", url, params)
	var response SkeletonResponse // TODO implement
	return response, nil
}

// GetStatus is x
func (d *DummyAccessor) GetStatus(basePath string, jobID JobID) (StatusResponse, error) {
	url := d.BuildGetStatusURL(basePath, jobID)
	return d.GetStatusByURL(url)
}

// GetStatusByURL is x
func (d *DummyAccessor) GetStatusByURL(url string) (StatusResponse, error) {
	log.Printf("Get: %s", url)
	response := StatusResponse{Status: Done}
	return response, nil
}

// BuildGetStatusURL is x
func (d *DummyAccessor) BuildGetStatusURL(basePath string, jobID JobID) string {
	return path.Join(basePath, "status", string(jobID))
}

// Analysis is x
type Analysis struct {
	TargetID string
	XID      string
	YID      string
	ZID      string
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

// NewDummyStateWalker is x
func NewDummyStateWalker(settings *ServiceSettings) *StateWalker {
	dummyAccessor := DummyAccessor{}
	sw := StateWalker{
		Settings: settings,
		gen:      NewOnMemoryObjectIDGenerator(),
		accessor: &dummyAccessor,
	}
	return &sw
}

// Sentinel is x
type Sentinel struct {
	endpoints []Endpoint
}

// Endpoint is x
type Endpoint struct {
	Service string
	URL     string
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
	for name, s := range states {
		if s.EndNode {
			endpoint := Endpoint{Service: name, URL: s.StatusAPIPath}
			sentinel.endpoints = append(sentinel.endpoints, endpoint)
		}
	}
	return sentinel
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

// StateWalker is x
type StateWalker struct {
	gen      ObjectIDGenerator
	accessor ExternalServiceAccessor
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

	// post data to external service
	response, err := sw.accessor.CreateSkeleton(spec.BasePath, jobID, params)
	_ = response
	if err != nil {
		be.WriteString(fmt.Sprintf("%s -> call api: %v", serviceName, err))
	}
	state = State{
		JobID:         jobID,
		Status:        Waiting,
		Params:        params,
		EndNode:       spec.EndNode,
		StatusAPIPath: sw.accessor.BuildGetStatusURL(spec.BasePath, jobID),
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

// FetchSentinelStatus is x
func (sw *StateWalker) FetchSentinelStatus(ws *WholeState) (bool, error) {
	alldone := true
	for _, endpoint := range ws.sentinel.endpoints {
		state, ok := ws.States[endpoint.Service]
		if !ok {
			log.Printf("check status %s is not found by service", endpoint.Service)
		}
		response, err := sw.accessor.GetStatusByURL(endpoint.URL)
		if err != nil {
			return false, err
		}
		if response.Status != Done {
			alldone = false
		}
		state.Status = response.Status
	}
	return alldone, nil
}

// State is x
type State struct {
	JobID         JobID
	Params        map[string]string // xxx
	Status        Status
	EndNode       bool
	StatusAPIPath string
}

// WholeState is x
type WholeState struct {
	sentinel *Sentinel
	States   map[string]State
	Analysis *Analysis
}

// NewWholeState is x
func NewWholeState(analysis *Analysis) *WholeState {
	ws := WholeState{States: make(map[string]State), Analysis: analysis}
	return &ws
}

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

// Status is x
type Status int

const (
	// Waiting is x
	Waiting Status = iota
	// Initialized is x
	Initialized
	// Pending is x
	Pending
	// Cancelled is x
	Cancelled
	// Errorf is x
	Error
	// Done is x
	Done
)

func main() {
	sw := NewDummyStateWalker(&Settings)
	analysis := Analysis{
		TargetID: "targetId",
		XID:      "xid",
		YID:      "yid",
		ZID:      "zid",
	}
	ws := NewWholeState(&analysis)

	// patten 1
	pattern1 := []string{"taskB0", "taskB4", "taskA3"}
	err := sw.WalkByNames(ws, pattern1)
	if err != nil {
		panic(err)
	}
	log.Printf("%#v\n", ws)
	log.Println("check sentinel status")

	alldone, err := sw.FetchSentinelStatus(ws)
	if err != nil {
		panic(err)
	}
	log.Printf("alldone=%t %#v\n", alldone, ws)

	buf, err := json.Marshal(Settings)
	if err != nil {
		panic(err)
	}
	fmt.Printf(string(buf))
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
			ItemDependencies:    []string{"TargetID"},
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
			ItemDependencies:    []string{"TargetID", "XID"},
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
			ItemDependencies:    []string{"YID"},
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
			ItemDependencies:    []string{"XID", "YID", "ZID"},
			EndNode:             true,
		},
		"taskA4": ServiceSpec{
			BasePath:            "/api/task/A4/",
			ServiceDependencies: []string{"TargetID", "XID"},
			ItemDependencies:    []string{},
			EndNode:             false,
		},
		"taskA5": ServiceSpec{
			BasePath:            "/api/task/A5/",
			ServiceDependencies: []string{},
			ItemDependencies:    []string{"TargetID", "YID"},
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
