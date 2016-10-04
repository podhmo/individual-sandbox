package deps

import (
	analysis "../analysis"
)

// WholeState is x
type WholeState struct {
	sentinel *Sentinel
	States   map[string]State
	Analysis *analysis.Analysis
}

// NewWholeState is x
func NewWholeState(analysis *analysis.Analysis) *WholeState {
	ws := WholeState{States: make(map[string]State), Analysis: analysis}
	return &ws
}

// Sentinel is x
type Sentinel struct {
	endpoints []Endpoint
}

// Endpoint is x
type Endpoint string

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
